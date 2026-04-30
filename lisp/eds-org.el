;;; eds-org.el --- small, custom tweaks to org-mode -*- lexical-binding: t; -*-

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
;; for my usage of org-mode.

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

;;;###autoload
(defun eds-org/get-org-directory ()
  "The location of my org directory varies by computer."
  (file-truename "~/Dropbox/org"))

;;;###autoload
(defun eds-org/get-conflicted-org-files ()
  "Search the org directory for files with \"conflicted\" in the name."
  (eds-utils/filter-for-regex "conflicted" (directory-files (eds-org/get-org-directory))))

(defun eds-org/get-link-from-link (link)
  "Extract the URL from an org-roam LINK."
  (if (string-match "\\[\\[\\(.*?\\)\\]\\[.*?\\]\\]" link)
      (match-string 1 link)
    nil))

(defun eds-org/get-org-file-name (title)
  "Get a suitable org-roam filename for TITLE."
  (let* ((time-string (eds-utils/get-time-string))
         (extension "org")
         (clean-title (eds-utils/strip-invalid-chars title))
         (slug (eds-utils/process-title clean-title))
         (file-base-name (concat time-string "-" slug "." extension)))
    (expand-file-name file-base-name (eds-org/get-org-directory))))

;;;###autoload
(defun eds-org/create-new-note-from-clipboard-link (title)
  "Create or update an org roam node with TITLE from a (presumable) url in the clipboard."
  (interactive "sTitle: ")
  (let* ((clipboard-content (or (gui-get-selection 'CLIPBOARD) "Clipboard is empty.")))
    (org-roam-protocol-open-ref
     `(:title ,title
       :ref ,clipboard-content
       :body ""
       :template "r"))))

;;;###autoload
(defun eds-org/set-category-value (value)
  "Set the CATEGORY property of the current node to VALUE."
  (interactive (list nil))
  (let ((value (or value (org-read-property-value "CATEGORY"))))
    (setq org-last-set-property "CATEGORY")
    (setq org-last-set-property-value (concat "CATEGORY: " value))
    (unless (equal (org-entry-get nil "CATEGORY") value)
      (save-excursion
        (beginning-of-buffer)
        (org-entry-put nil "CATEGORY" value)))))

;;;###autoload
(defun eds-org/capture-email (msg)
  "Capture an email message MSG into org-roam."
  (interactive)
  (let* ((link (org-store-link msg nil))
         (ref  (eds-org/get-link-from-link link))
         (subject (eds-email/get-subject-from-msg msg))
         (body (if mark-active
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 "")))
    (org-roam-protocol-open-ref
     `(:title ,subject
       :ref ,ref
       :body ,body
       :template "r"))))

;;;###autoload
(defun eds-org/capture-email-todo (msg)
  "Capture an email message MSG into a TODO in org-roam."
  (interactive)
  (let* ((link (org-store-link msg nil))
         (ref  (eds-org/get-link-from-link link))
         (subject (eds-email/get-subject-from-msg msg))
         (body (if mark-active
                   (buffer-substring-no-properties (region-beginning) (region-end))
                  "")))
    (org-roam-protocol-open-ref
     `(:title ,subject
       :ref ,ref
       :body ,body
       :template "T"))))

;;;###autoload
(defun eds-org/get-org-agenda-files ()
  "Return a list of org files containing the :agenda: tag, using grep and shell glob expansion."
  (let* ((org-directory (eds-org/get-org-directory))
         (default-directory (concat org-directory "/"))
         (cmd "grep -l ':agenda:' *.org")
         (output (shell-command-to-string cmd)))
    (mapcar (lambda (file) (expand-file-name file org-directory))
            (split-string output "\n" t))))

;; Got this idea (slightly modified) from https://github.com/jwiegley/dot-emacs.
(defun eds-org/query-for-agenda-tagged-files ()
  "Return a list of note files containing `agenda' tag." ;
  (seq-uniq
   (seq-map
    #'vulpea-note-path
    (vulpea-db-query-by-tags-some '("agenda")))))

;;;###autoload
(defun eds-org/update-agenda-files (&rest _)
  "Update the value of `org-agenda-files'."
  (interactive)
  (let* ((org-directory (eds-org/get-org-directory))
         (eds-org-calendar-file (concat org-directory "/calendar.org")))
    (setq org-agenda-files (eds-org/query-for-agenda-tagged-files))
    (add-to-list 'org-agenda-files eds-org-calendar-file)
    (message "Updated org-agenda-files: %s" (length org-agenda-files))))

;;;###autoload
(defun eds-org/org-roam-graph-small ()
  (interactive)
  (org-roam-graph 2 (org-roam-node-at-point)))

(defun eds-org/remove-title-boilerplate (title)
  "Strip some extraneous suffixes and prefixes from TITLE."
  (thread-last title
               (replace-regexp-in-string " - Dropbox Paper$" "")
               (replace-regexp-in-string " – Dropbox Paper$" "") ;; some titles have an en dash instead of a hyphen
               (replace-regexp-in-string " - BBC Jira Cloud$" "")
               (replace-regexp-in-string " - Passports - Confluence$" "")
               (replace-regexp-in-string " - Product Group - Confluence$" "")
               (replace-regexp-in-string " - Miro$" "")
               (replace-regexp-in-string " - Passports - BBC Confluence Cloud$" "")
               (replace-regexp-in-string "^Richard Rohr’s Daily Meditation: " "")))

(provide 'eds-org)

;;; eds-org.el ends here
