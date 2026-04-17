;;; eds-utils.el --- Utility functions and initialisation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan <me@eamonnsullivan.co.uk>

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Maintainer: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Created: 2024-03-15
;; Version: 0.1
;; Package-Requires: ((emacs "31.0"))
;; Keywords: convenience, tools, utilities
;; URL: https://github.com/eamonnsullivan/eds-utils

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides initialisation and a collection of utility functions
;; for Emacs, enhancing convenience and workflow.

;;; Licence:

;; This programme is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public Licence as published by
;; the Free Software Foundation, either version 3 of the Licence, or
;; (at your option) any later version.

;; This programme is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public Licence for more details.

;; You should have received a copy of the GNU General Public Licence
;; along with this programme.  If not, see <https://www.gnu.org/licenses/>.

(require 'eds-org)

(defun eds-utils/extract-jira-ticket (input)
  "Extract a jira ticket from INPUT, or some other well-known prefixes."
  (cond
   ((string-match "[A-Za-z]+-[0-9]+" input) (upcase (match-string 0 input)))
   ((string-match "^innovation+" input) "INNOVATION-DAY")
   ((string-match "^cop-" input) "COP-DAY")
   (t "NO-TICKET")))

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun eds-utils/backward-kill-word (arg)
  "Delete chars backward until the end of a word. With ARG, do it that many times."
  (interactive "p")
  (eds-utils/kill-word (- arg)))

;;;###autoload
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

;;;###autoload
(defun eds-utils/kill-emacs ()
  "Kill EMACS."
  (if (daemonp)
      (save-buffers-kill-emacs)
    (save-buffers-kill-terminal)))

;;;###autoload
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

(defun eds-utils/visit-term ()
  "If current buffer is the default `*vterm*`, prompt to rename it.
Otherwise, create a new vterm buffer with the default base name."
  (interactive)
  (if (and (derived-mode-p 'vterm-mode)
           (string= (buffer-name) "*vterm*"))
      (let ((new-name (read-string "Rename *vterm* to: ")))
        (unless (string-empty-p new-name)
          (rename-buffer new-name t)))
    (vterm)))

(provide 'eds-utils)
;;; eds-utils.el ends here
