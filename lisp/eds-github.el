;;; eds-github.el --- GitHub integration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan <me@eamonnsullivan.co.uk>

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Maintainer: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, github, ci, pull-requests
;; URL: https://github.com/eamonnsullivan/emacs.d

;; This file is not part of GNU Emacs.

;;; Commentary:

;; List GitHub Actions workflow runs in a tabulated buffer.
;; View and filter pull requests with in-buffer narrowing.
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

(defgroup eds-github nil
  "GitHub integration for Emacs."
  :group 'tools)

(defvar eds-github/--current-repo nil
  "The repository currently displayed in the runs buffer.")

(defcustom eds-github/run-limit 20
  "Maximum number of workflow runs to fetch."
  :type 'integer
  :group 'eds-github)

(defface eds-github/success-face
  '((t :inherit success :weight bold))
  "Face for successful workflow runs."
  :group 'eds-github)

(defface eds-github/failure-face
  '((t :inherit error :weight bold))
  "Face for failed workflow runs."
  :group 'eds-github)

(defface eds-github/in-progress-face
  '((t :inherit warning :weight bold))
  "Face for in-progress workflow runs."
  :group 'eds-github)

(defface eds-github/cancelled-face
  '((t :inherit shadow))
  "Face for cancelled workflow runs."
  :group 'eds-github)

(defun eds-github/--gh-json (error-prefix &rest args)
  "Run gh with ARGS and parse JSON output.
ERROR-PREFIX is used when the gh CLI exits non-zero."
  (with-temp-buffer
    (let ((exit-code (apply #'process-file "gh" nil t nil args))
          (output (string-trim (buffer-string))))
      (unless (zerop exit-code)
        (error "%s: %s" error-prefix output))
      (if (string-empty-p output)
          (error "No output from gh CLI — is it installed and authenticated?")
        (json-parse-string output :object-type 'alist)))))

(defun eds-github/--alist-string (key alist)
  "Return string value for KEY in ALIST, or the empty string."
  (let ((value (alist-get key alist)))
    (if (and value (not (eq value json-null)) (stringp value))
        value
      "")))

(defun eds-github/--nested-alist-string (object-key value-key alist)
  "Return nested string VALUE-KEY from OBJECT-KEY in ALIST, or empty string."
  (let ((object (alist-get object-key alist)))
    (if (consp object)
        (eds-github/--alist-string value-key object)
      "")))

(defun eds-github/--number-string (number)
  "Return NUMBER as a string, or empty string when nil."
  (if number (number-to-string number) ""))

(defun eds-github/--set-tabulated-entries (items formatter)
  "Set `tabulated-list-entries' from ITEMS using FORMATTER."
  (setq tabulated-list-entries
        (mapcar formatter (append items nil))))

(defun eds-github/--setup-tabulated-list (format sort-key refresh-function)
  "Set common tabulated-list variables.
FORMAT is assigned to `tabulated-list-format', SORT-KEY is assigned
to `tabulated-list-sort-key', and REFRESH-FUNCTION is added to the
buffer-local `tabulated-list-revert-hook'."
  (setq tabulated-list-format format)
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key sort-key)
  (add-hook 'tabulated-list-revert-hook refresh-function nil t)
  (tabulated-list-init-header))

(defun eds-github/--id-at-point-or-error (message)
  "Return `tabulated-list-get-id' or signal user error MESSAGE."
  (or (tabulated-list-get-id)
      (user-error "%s" message)))

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
  (eds-github/--gh-json "GitHub CLI run list failed"
                        "run" "list"
                        "--repo" repo
                        "--json" "databaseId,name,status,conclusion,headBranch,event,startedAt"
                        "--limit" (number-to-string eds-github/run-limit)))

(defun eds-github/--format-entry (run)
  "Format a single RUN alist into a `tabulated-list-entries' row."
  (let* ((id (alist-get 'databaseId run))
         (status (eds-github/--alist-string 'status run))
         (conclusion (eds-github/--alist-string 'conclusion run))
         (display-status (if (string= status "completed") conclusion status))
         (face (eds-github/--status-face status conclusion)))
    (list id
          (vector (propertize display-status 'face face)
                  (eds-github/--alist-string 'name run)
                  (eds-github/--alist-string 'headBranch run)
                  (eds-github/--alist-string 'event run)
                  (eds-github/--format-time
                   (eds-github/--alist-string 'startedAt run))))))

(defun eds-github/--refresh ()
  "Refresh the workflow runs list."
  (eds-github/--set-tabulated-entries
   (eds-github/--fetch-runs eds-github/--current-repo)
   #'eds-github/--format-entry))

(defun eds-github/view-run-at-point ()
  "Open the workflow run at point in the browser."
  (interactive)
  (let ((id (eds-github/--id-at-point-or-error "No run at point")))
    (shell-command
     (format "gh run view %d --repo %s --web"
             id
             (shell-quote-argument eds-github/--current-repo)))))

(defun eds-github/rerun-at-point ()
  "Rerun the workflow run at point."
  (interactive)
  (let ((id (eds-github/--id-at-point-or-error "No run at point")))
    (when (yes-or-no-p (format "Rerun workflow %d?" id))
      (shell-command
       (format "gh run rerun %d --repo %s"
               id
               (shell-quote-argument eds-github/--current-repo)))
      (revert-buffer))))

(defun eds-github/cancel-at-point (force)
  "Cancel the workflow run at point.
With prefix argument FORCE, pass --force to cancel even if completed."
  (interactive "P")
  (let ((id (eds-github/--id-at-point-or-error "No run at point")))
    (when (yes-or-no-p (format "Cancel workflow %d%s?"
                               id (if force " (force)" "")))
      (shell-command
       (format "gh run cancel %d --repo %s%s"
               id
               (shell-quote-argument eds-github/--current-repo)
               (if force " --force" "")))
      (revert-buffer))))

(defvar eds-github/runs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'eds-github/view-run-at-point)
    (define-key map (kbd "r") #'eds-github/rerun-at-point)
    (define-key map (kbd "c") #'eds-github/cancel-at-point)
    map)
  "Keymap for `eds-github/runs-mode'.")

(define-derived-mode eds-github/runs-mode tabulated-list-mode "GH-Runs"
  "Major mode for listing GitHub Actions workflow runs."
  (eds-github/--setup-tabulated-list
   [("Status" 12 t)
    ("Name" 35 t)
    ("Branch" 25 t)
    ("Event" 12 t)
    ("Started" 16 t)]
   '("Started" . t)
   #'eds-github/--refresh))

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

;;; -----------------------------------------------------------------
;;; Pull Requests
;;; -----------------------------------------------------------------

(defcustom eds-github/pr-limit 30
  "Maximum number of pull requests to fetch."
  :type 'integer
  :group 'eds-github)

(defcustom eds-github/pr-default-state "open"
  "Default state filter for pull requests."
  :type '(choice (const :tag "Open" "open")
                 (const :tag "Closed" "closed")
                 (const :tag "All" "all"))
  :group 'eds-github)

(defvar-local eds-github/--pr-repo nil
  "The repository currently displayed in the PRs buffer.")

(defvar-local eds-github/--pr-state nil
  "Current state filter for the PRs buffer.")

(defvar-local eds-github/--pr-author nil
  "Current author filter for the PRs buffer (nil means all).")

(defun eds-github/--read-pr-state ()
  "Read a pull request state filter from the minibuffer."
  (completing-read "PR state: " '("open" "closed" "all") nil t))

(defun eds-github/--set-pr-state (state)
  "Set the current PR STATE filter and refresh the buffer."
  (setq-local eds-github/--pr-state state)
  (revert-buffer))

(defun eds-github/--reset-pr-state ()
  "Reset the current PR state filter to `eds-github/pr-default-state'."
  (setq-local eds-github/--pr-state eds-github/pr-default-state))

(defface eds-github/pr-open-face
  '((t :inherit success :weight bold))
  "Face for open pull requests."
  :group 'eds-github)

(defface eds-github/pr-closed-face
  '((t :inherit error :weight bold))
  "Face for closed pull requests."
  :group 'eds-github)

(defface eds-github/pr-merged-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for merged pull requests."
  :group 'eds-github)

(defun eds-github/--pr-state-face (state)
  "Return a face for the given PR STATE string."
  (cond
   ((string= state "OPEN") 'eds-github/pr-open-face)
   ((string= state "CLOSED") 'eds-github/pr-closed-face)
   ((string= state "MERGED") 'eds-github/pr-merged-face)
   (t 'default)))

(defun eds-github/--propertized-pr-state (state &optional upcase)
  "Return STATE propertized with its PR state face.
When UPCASE is non-nil, uppercase STATE before choosing the face."
  (let ((state (if upcase
                   (upcase (or state ""))
                 (or state ""))))
    (propertize state 'face (eds-github/--pr-state-face state))))

(defun eds-github/--fetch-prs (repo state &optional author)
  "Fetch pull requests for REPO filtered by STATE and optionally AUTHOR.
STATE should be \"open\", \"closed\", or \"all\".
Returns a parsed JSON vector of PR objects."
  (let* ((args (list "pr" "list"
                     "--repo" repo
                     "--json" "number,title,state,author,headRefName,createdAt,updatedAt"
                     "--state" state
                     "--limit" (number-to-string eds-github/pr-limit)))
         (args (if (and author (not (string-empty-p author)))
                   (append args (list "--author" author))
                 args)))
    (apply #'eds-github/--gh-json "GitHub CLI pr list failed" args)))

(defun eds-github/--format-pr-entry (pr)
  "Format a single PR alist into a `tabulated-list-entries' row."
  (let ((number (alist-get 'number pr))
        (state (eds-github/--alist-string 'state pr)))
    (list number
          (vector (number-to-string number)
                  (eds-github/--propertized-pr-state state)
                  (eds-github/--alist-string 'title pr)
                  (eds-github/--nested-alist-string 'author 'login pr)
                  (eds-github/--alist-string 'headRefName pr)
                  (eds-github/--format-time
                   (eds-github/--alist-string 'updatedAt pr))))))

(defun eds-github/--pr-header-line ()
  "Build a header-line string showing active PR filters."
  (let ((parts (list (format "PRs for %s" eds-github/--pr-repo))))
    (when eds-github/--pr-state
      (push (format "state: %s" eds-github/--pr-state) parts))
    (when eds-github/--pr-author
      (push (format "author: %s" eds-github/--pr-author) parts))
    (if (= (length parts) 1)
        (car parts)
      (format "%s [%s]"
              (car (last parts))
              (string-join (butlast parts) ", ")))))

(defun eds-github/--refresh-prs ()
  "Refresh the pull requests list using current filters."
  (eds-github/--set-tabulated-entries
   (eds-github/--fetch-prs eds-github/--pr-repo
                           (or eds-github/--pr-state
                               eds-github/pr-default-state)
                           eds-github/--pr-author)
   #'eds-github/--format-pr-entry)
  (setq header-line-format (eds-github/--pr-header-line)))

(defun eds-github/pr-filter-state (state)
  "Filter pull requests by STATE and refresh the list.
STATE should be \"open\", \"closed\", or \"all\"."
  (interactive (list (eds-github/--read-pr-state)))
  (eds-github/--set-pr-state state))

(defun eds-github/pr-filter-author (author)
  "Filter pull requests by AUTHOR and refresh the list.
If AUTHOR is empty, clear the author filter."
  (interactive (list (read-string "Author (blank to clear): ")))
  (setq-local eds-github/--pr-author
              (if (string-empty-p author) nil author))
  (revert-buffer))

(defun eds-github/pr-clear-filters ()
  "Clear all PR filters and refresh the list."
  (interactive)
  (eds-github/--reset-pr-state)
  (setq-local eds-github/--pr-author nil)
  (revert-buffer))

(defun eds-github/pr-view-at-point ()
  "Open the pull request at point in the browser."
  (interactive)
  (let ((number (eds-github/--id-at-point-or-error "No PR at point")))
    (shell-command
     (format "gh pr view %d --repo %s --web"
             number
             (shell-quote-argument eds-github/--pr-repo)))))

(defvar eds-github/prs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'eds-github/pr-view-at-point)
    (define-key map (kbd "/ s") #'eds-github/pr-filter-state)
    (define-key map (kbd "/ a") #'eds-github/pr-filter-author)
    (define-key map (kbd "/ /") #'eds-github/pr-clear-filters)
    map)
  "Keymap for `eds-github/prs-mode'.")

(define-derived-mode eds-github/prs-mode tabulated-list-mode "GH-PRs"
  "Major mode for listing GitHub pull requests."
  (eds-github/--setup-tabulated-list
   [("Num" 6 t)
    ("State" 8 t)
    ("Title" 45 t)
    ("Author" 16 t)
    ("Branch" 25 t)
    ("Updated" 16 t)]
   '("Updated" . t)
   #'eds-github/--refresh-prs))

;;;###autoload
(defun eds-github/list-prs (repo)
  "List GitHub pull requests for REPO.
REPO should be in owner/name format (e.g. \"bbc/arco\").
Defaults to the current repository's GitHub remote if available.
Use \\`/ s' to filter by state, \\`/ a' to filter by author,
and \\`/ /' to clear all filters."
  (interactive
   (list (read-string "Repository (owner/name): "
                      (eds-github/--default-repo))))
  (when (string-empty-p repo)
    (user-error "Repository name is required"))
  (let ((buf (get-buffer-create (format "*GitHub PRs: %s*" repo))))
    (with-current-buffer buf
      (eds-github/prs-mode)
      (setq-local eds-github/--pr-repo repo)
      (setq-local eds-github/--pr-state eds-github/pr-default-state)
      (setq-local eds-github/--pr-author nil)
      (eds-github/--refresh-prs)
      (tabulated-list-print t))
    (pop-to-buffer buf)))

;;; My pull requests (across all repositories)

(defun eds-github/--fetch-my-prs (state)
  "Fetch the authenticated user's pull requests filtered by STATE.
STATE should be \"open\", \"closed\", or \"all\".  When STATE is
\"all\", no state qualifier is passed to `gh search'.
Returns a parsed JSON vector of PR objects."
  (let* ((args (list "search" "prs"
                     "--author" "@me"
                     "--json" "number,title,state,repository,updatedAt,url"
                     "--limit" (number-to-string eds-github/pr-limit)))
         (args (if (and state (not (string= state "all")))
                   (append args (list "--state" state))
                 args)))
    (apply #'eds-github/--gh-json "GitHub CLI search prs failed" args)))

(defun eds-github/--format-my-pr-entry (pr)
  "Format a single PR alist from `gh search prs' into a row.
The row id is the PR URL so it can be opened across repositories."
  (let ((state (eds-github/--alist-string 'state pr)))
    (list (eds-github/--alist-string 'url pr)
          (vector (eds-github/--nested-alist-string 'repository 'nameWithOwner pr)
                  (eds-github/--number-string (alist-get 'number pr))
                  (eds-github/--propertized-pr-state state t)
                  (eds-github/--alist-string 'title pr)
                  (eds-github/--format-time
                   (eds-github/--alist-string 'updatedAt pr))))))

(defun eds-github/--my-pr-header-line ()
  "Build a header-line string showing the active my-PRs filter."
  (format "My PRs [state: %s]"
          (or eds-github/--pr-state eds-github/pr-default-state)))

(defun eds-github/--refresh-my-prs ()
  "Refresh the my-pull-requests list using the current state filter."
  (eds-github/--set-tabulated-entries
   (eds-github/--fetch-my-prs
    (or eds-github/--pr-state eds-github/pr-default-state))
   #'eds-github/--format-my-pr-entry)
  (setq header-line-format (eds-github/--my-pr-header-line)))

(defun eds-github/my-pr-filter-state (state)
  "Filter the my-pull-requests list by STATE and refresh.
STATE should be \"open\", \"closed\", or \"all\"."
  (interactive (list (eds-github/--read-pr-state)))
  (eds-github/--set-pr-state state))

(defun eds-github/my-pr-clear-filters ()
  "Reset the my-pull-requests state filter to the default and refresh."
  (interactive)
  (eds-github/--reset-pr-state)
  (revert-buffer))

(defun eds-github/my-pr-view-at-point ()
  "Open the pull request at point in the browser."
  (interactive)
  (let ((url (eds-github/--id-at-point-or-error "No PR at point")))
    (if (not (string-empty-p url))
        (browse-url url)
      (user-error "No PR at point"))))

(defvar eds-github/my-prs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'eds-github/my-pr-view-at-point)
    (define-key map (kbd "/ s") #'eds-github/my-pr-filter-state)
    (define-key map (kbd "/ /") #'eds-github/my-pr-clear-filters)
    map)
  "Keymap for `eds-github/my-prs-mode'.")

(define-derived-mode eds-github/my-prs-mode tabulated-list-mode "GH-MyPRs"
  "Major mode for listing the authenticated user's pull requests."
  (eds-github/--setup-tabulated-list
   [("Repo" 30 t)
    ("Num" 6 t)
    ("State" 8 t)
    ("Title" 50 t)
    ("Updated" 16 t)]
   '("Updated" . t)
   #'eds-github/--refresh-my-prs))

;;;###autoload
(defun eds-github/list-my-prs ()
  "List the authenticated user's open GitHub pull requests across all repos.
Uses `gh search prs --author @me'.  Use \\`/ s' to filter by state
and \\`/ /' to reset the filter."
  (interactive)
  (let ((buf (get-buffer-create "*GitHub My PRs*")))
    (with-current-buffer buf
      (eds-github/my-prs-mode)
      (setq-local eds-github/--pr-state eds-github/pr-default-state)
      (eds-github/--refresh-my-prs)
      (tabulated-list-print t))
    (pop-to-buffer buf)))

(provide 'eds-github)
;;; eds-github.el ends here
