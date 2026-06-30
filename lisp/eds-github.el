;;; eds-github.el --- GitHub Actions integration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan <me@eamonnsullivan.co.uk>

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Maintainer: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, github, ci
;; URL: https://github.com/eamonnsullivan/emacs.d

;; This file is not part of GNU Emacs.

;;; Commentary:

;; List GitHub Actions workflow runs in a tabulated buffer.
;; Requires the `gh` CLI tool to be installed and authenticated.

;;; Licence:

;; This programme is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public Licence as published by
;; the Free Software Foundation, either version 3 of the Licence, or
;; (at your option) any later version.

;;; Code:

(require 'json)
(require 'subr-x)
(require 'tabulated-list)

(defvar eds-github/--current-repo nil
  "The repository currently displayed in the runs buffer.")

(defvar eds-github/run-limit 20
  "Maximum number of workflow runs to fetch.")

(defface eds-github/success-face
  '((t :foreground "#50fa7b" :weight bold))
  "Face for successful workflow runs."
  :group 'eds-github)

(defface eds-github/failure-face
  '((t :foreground "#ff5555" :weight bold))
  "Face for failed workflow runs."
  :group 'eds-github)

(defface eds-github/in-progress-face
  '((t :foreground "#f1fa8c" :weight bold))
  "Face for in-progress workflow runs."
  :group 'eds-github)

(defface eds-github/cancelled-face
  '((t :foreground "#6272a4"))
  "Face for cancelled workflow runs."
  :group 'eds-github)

(defun eds-github/--status-face (status conclusion)
  "Return a face based on STATUS and CONCLUSION."
  (cond
   ((string= status "in_progress") 'eds-github/in-progress-face)
   ((string= status "queued") 'eds-github/in-progress-face)
   ((string= conclusion "success") 'eds-github/success-face)
   ((string= conclusion "failure") 'eds-github/failure-face)
   ((string= conclusion "cancelled") 'eds-github/cancelled-face)
   (t 'default)))

(defun eds-github/--format-time (iso-time)
  "Format ISO-TIME string into a shorter relative or absolute form."
  (if (or (null iso-time) (string-empty-p iso-time))
      ""
    (format-time-string "%Y-%m-%d %H:%M"
                        (date-to-time iso-time))))

(defun eds-github/--fetch-runs (repo)
  "Fetch workflow runs for REPO using the gh CLI.
Returns a parsed JSON vector of run objects."
  (with-temp-buffer
    (let ((exit-code (process-file "gh" nil t nil
                                   "run" "list"
                                   "--repo" repo
                                   "--json" "databaseId,name,status,conclusion,headBranch,event,startedAt"
                                   "--limit" (number-to-string eds-github/run-limit)))
          (output (string-trim (buffer-string))))
      (unless (zerop exit-code)
        (error "GitHub CLI run list failed: %s" output))
      (if (string-empty-p output)
          (error "No output from gh CLI — is it installed and authenticated?")
        (json-parse-string output :object-type 'alist)))))

(defun eds-github/--format-entry (run)
  "Format a single RUN alist into a `tabulated-list-entries' row."
  (let* ((id (alist-get 'databaseId run))
         (name (or (alist-get 'name run) ""))
         (status (or (alist-get 'status run) ""))
         (conclusion (or (alist-get 'conclusion run) ""))
         (branch (or (alist-get 'headBranch run) ""))
         (event (or (alist-get 'event run) ""))
         (started (eds-github/--format-time (or (alist-get 'startedAt run) "")))
         (display-status (if (string= status "completed") conclusion status))
         (face (eds-github/--status-face status conclusion)))
    (list id
          (vector (propertize display-status 'face face)
                  name
                  branch
                  event
                  started))))

(defun eds-github/--refresh ()
  "Refresh the workflow runs list."
  (let ((runs (eds-github/--fetch-runs eds-github/--current-repo)))
    (setq tabulated-list-entries
          (mapcar #'eds-github/--format-entry
                  (append runs nil)))))

(defun eds-github/view-run-at-point ()
  "Open the workflow run at point in the browser."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (if id
        (shell-command
         (format "gh run view %d --repo %s --web"
                 id
                 (shell-quote-argument eds-github/--current-repo)))
      (user-error "No run at point"))))

(defun eds-github/rerun-at-point ()
  "Rerun the workflow run at point."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (if id
        (when (yes-or-no-p (format "Rerun workflow %d?" id))
          (shell-command
           (format "gh run rerun %d --repo %s"
                   id
                   (shell-quote-argument eds-github/--current-repo)))
          (revert-buffer))
      (user-error "No run at point"))))

(defun eds-github/cancel-at-point (force)
  "Cancel the workflow run at point.
With prefix argument FORCE, pass --force to cancel even if completed."
  (interactive "P")
  (let ((id (tabulated-list-get-id)))
    (if id
        (when (yes-or-no-p (format "Cancel workflow %d%s?"
                                   id (if force " (force)" "")))
          (shell-command
           (format "gh run cancel %d --repo %s%s"
                   id
                   (shell-quote-argument eds-github/--current-repo)
                   (if force " --force" "")))
          (revert-buffer))
      (user-error "No run at point"))))

(defvar eds-github/runs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'eds-github/view-run-at-point)
    (define-key map (kbd "r") #'eds-github/rerun-at-point)
    (define-key map (kbd "c") #'eds-github/cancel-at-point)
    map)
  "Keymap for `eds-github/runs-mode'.")

(define-derived-mode eds-github/runs-mode tabulated-list-mode "GH-Runs"
  "Major mode for listing GitHub Actions workflow runs."
  (setq tabulated-list-format
        [("Status" 12 t)
         ("Name" 35 t)
         ("Branch" 25 t)
         ("Event" 12 t)
         ("Started" 16 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Started" . t))
  (add-hook 'tabulated-list-revert-hook #'eds-github/--refresh nil t)
  (tabulated-list-init-header))

(defun eds-github/--default-repo ()
  "Attempt to determine the repo from the current git remote."
  (let ((url (ignore-errors
               (car (process-lines "git" "config" "--get" "remote.origin.url")))))
    (when (and url (string-match "github\\.com[:/]\\(.+?\\)\\(?:\\.git\\)?$" url))
      (match-string 1 url))))

;;;###autoload
(defun eds-github/list-runs (repo)
  "List GitHub Actions workflow runs for REPO.
REPO should be in owner/name format (e.g. \"bbc/arco\").
Defaults to the current repository's GitHub remote if available."
  (interactive
   (list (read-string "Repository (owner/name): "
                      (eds-github/--default-repo))))
  (when (string-empty-p repo)
    (user-error "Repository name is required"))
  (let ((buf (get-buffer-create (format "*GitHub Runs: %s*" repo))))
    (with-current-buffer buf
      (eds-github/runs-mode)
      (setq-local eds-github/--current-repo repo)
      (eds-github/--refresh)
      (tabulated-list-print t))
    (pop-to-buffer buf)))

(provide 'eds-github)
;;; eds-github.el ends here
