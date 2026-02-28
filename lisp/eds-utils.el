;;; eds-utils.el --- Library of small functions, mostly used by my own stuff  -*- lexical-binding:t -*-
;;
;; Copyright (C) 2026 Eamonnn Sullivan
;;
;;
;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Version: 1.0
;; Keywords: emacs utils
;; URL: https://eamonnsullivan.co.uk
;;
;;
;;; Commentary:
;;
;;
;;; Code:

(require 'eds-org)

(defun eds-utils/extract-jira-ticket (input)
  "Extract a jira ticket from INPUT, or some other well-known prefixes."
  (cond
   ((string-match "[A-Za-z]+-[0-9]+" input) (upcase (match-string 0 input)))
   ((string-match "^innovation+" input) "INNOVATION-DAY")
   ((string-match "^cop-" input) "COP-DAY")
   (t "NO-TICKET")))

(defun eds-utils/insert-git-branch-name ()
  "Insert the current git branch name at point, surrounded by square brackets.
We use this to make the jira tickets easy to spot in the commit messages."
  (interactive)
  (when eds-insert-branch-name-p
    (setopt eds-insert-branch-name-p nil)
    (insert
     "["
     (eds-utils/extract-jira-ticket
      (magit-get-current-branch))
     "] ")))

(defun eds-utils/kill-word (arg)
  "Delete chars forward until the end of a word. With ARG, do this that many times."
  (interactive "p")
  (if (use-region-p)
      (delete-region
       (region-beginning)
       (region-end))
    (delete-region
     (point)
     (progn
       (forward-word arg)
       (point)))))

(defun eds-utils/backward-kill-word (arg)
  "Delete chars backward until the end of a word. With ARG, do it that many times."
  (interactive "p")
  (eds-utils/kill-word (- arg)))

(defun eds-utils/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer
   (other-buffer
    (current-buffer)
    1)))

(defun eds-utils/open-buffer-on-desktop ()
  "Open the current directory in Mac OS X finder, Nautilus or Explorer."
  (interactive)
  (let ((file (buffer-file-name))
        (cmd (cond ((eq system-type 'darwin)
                    "open")
                   ((eq system-type 'gnu/linux)
                    "nautilus")
                   ((eq system-type 'windows-nt)
                    "explorer")
                   ;; haven't tested this
                   (t nil))))
    (if file
        (if cmd
            (shell-command
             (format
              "%s %s"
              (executable-find cmd)
              (file-name-directory file)))
          (display-warning
           :error "Unknown system type"))
      (display-warning
       :error "Buffer not attached to any file"))))

(defun eds-utils/strip-invalid-chars (title)
  "Strip characters from TITLE that don't work in filenames or github branches."
  (replace-regexp-in-string "[\\?\\>\\<\\|\\:\\&\\*\/\\!]" "" title))

(defun eds-utils/process-title (title)
  "Downcase and hyphenate TITLE to case string.
Remove characters that don't work in a filename."
  (eds-utils/strip-invalid-chars (mapconcat 'identity (split-string (downcase title)) "-")))

(defun eds-utils/filter-for-regex (regex strings)
  "Filter STRINGS (a list of strings) to those matching REGEX."
  (seq-filter (lambda (str) (string-match-p regex str)) strings))

(defun eds-utils/dashboard-custom-conflicted-files (list-size)
  "Custom dashboard element listing any conflicts in my org files. LIST-SIZE is the number of lines to show before truncating."
  (if (> (length (eds-org/get-conflicted-org-files)) 0)
      (progn
        (dashboard-insert-heading "Dropbox:")
        (insert " There are some dropbox errors in our org files.")
        (insert "\n")
        (dolist (str (eds-org/get-conflicted-org-files))
          (insert str)))
    (progn
      (dashboard-insert-heading "Dropbox:")
      (insert " No dropbox conficts found in our org files."))
    ))

(defun eds-utils/get-time-string ()
  "Get the current time as a string formatted for org-roam filenames."
  (format-time-string "%Y%m%dT%H%M%S"))

(defun eds-utils/kill-emacs ()
  "Kill EMACS."
  (if (daemonp)
      (save-buffers-kill-emacs)
    (save-buffers-kill-terminal)))

(defun eds-utils/restart-emacs (arg)
  "Close EMACS, asking for confirmation. With a prefix ARG, restart it."
  (interactive "P")
  (let ((confirm-kill-emacs (unless arg 'y-or-n-p))
        (kill-emacs-query-functions
         (if arg
             (append (list
                      (lambda ()
                        (when (y-or-n-p (format "Really restart %s? "
                                                (capitalize invocation-name)))
                          (add-hook 'kill-emacs-hook
                                    (lambda ()
                                      (call-process-shell-command
                                       (format "(%s &)"
                                               (or (executable-find "emacs")
                                                   (executable-find "Emacs")))))
                                    t))))
                     kill-emacs-query-functions)
           kill-emacs-query-functions)))
    (eds-utils/kill-emacs)))

(defun eds-utils/filter-buffer-list (buflist)
  "Filter BUFLIST to exclude buffers I want to keep when clearing all buffers.."
  (let ((buffers-to-keep '("*scratch*" "*pomidor*" "*dashboard*")))
    (seq-filter (lambda (buf)
                  (not (member (buffer-name buf) buffers-to-keep)))
                buflist)))

(provide 'eds-utils)
;;; eds-utils.el ends here
