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
  "Extract the URL from an org-roam LINK.

The brackets are kept, because org-roam only accepts `ROAM_REFS'
values that match `org-link-any-re'.  A bare `mu4e:msgid:...' string
does not match, while the bracketed form does."
  (if (string-match "\\[\\[\\(.*?\\)\\]\\[.*?\\]\\]" link)
      (match-string 0 link)
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

(defvar eds-org/category-history nil
  "History of categories entered when capturing Org-roam nodes.")

(defun eds-org/explicit-category ()
  "Return an explicitly set category at point, or nil.
Check the current entry, its ancestors, the file property drawer and
#+CATEGORY keywords.  Never return Org's filename-derived fallback."
  (save-excursion
    (save-restriction
      (widen)
      (org-back-to-heading-or-point-min t)
      (let ((case-fold-search t)
            category)
        (while
            (progn
              (when-let* ((range (org-get-property-block)))
                (setq category
                      (save-excursion
                        (goto-char (car range))
                        (when (re-search-forward
                               "^[ \t]*:CATEGORY:[ \t]*\\(.*\\)$" (cdr range) t)
                          (org-trim (match-string-no-properties 1))))))
              (and (not (org-string-nw-p category))
                   (> (point) (point-min))
                   (or (org-up-heading-safe)
                       (goto-char (point-min))))))
        (or (and (org-string-nw-p category) category)
            (seq-find #'org-string-nw-p
                      (cdr (assoc "CATEGORY"
                                  (org-collect-keywords '("CATEGORY"))))))))))

(defun eds-org/read-category ()
  "Read a nonempty category with completion, allowing new values."
  (let ((completion-ignore-case t)
        (categories (delete-dups
                     (append (org-property-values "CATEGORY")
                             (org-property-get-allowed-values nil "CATEGORY")
                             eds-org/category-history)))
        category)
    (while (not (org-string-nw-p category))
      (setq category
            (org-trim
             (completing-read "Category: " categories nil nil nil
                              'eds-org/category-history))))
    category))

;;;###autoload
(defun eds-org/ensure-capture-category ()
  "Prompt for CATEGORY at the capture target unless explicitly set.
Suitable for `org-roam-capture-new-node-hook'; preserve point and narrowing."
  (save-excursion
    (save-restriction
      (widen)
      (unless (eds-org/explicit-category)
        (org-entry-put nil "CATEGORY" (eds-org/read-category))))))

;;;###autoload
(defun eds-org/set-category-value (value)
  "Set the file-level CATEGORY property to VALUE, prompting if nil."
  (interactive (list nil))
  (let ((value (or value (eds-org/read-category))))
    (setq org-last-set-property "CATEGORY")
    (setq org-last-set-property-value (concat "CATEGORY: " value))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
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
               (replace-regexp-in-string " - Security Architecture - Confluence$" "")
               (replace-regexp-in-string " - Confluence$" "")
               (replace-regexp-in-string " - Miro$" "")
               (replace-regexp-in-string " - Passports - BBC Confluence Cloud$" "")
               (replace-regexp-in-string "^Richard Rohr’s Daily Meditation: " "")))

(defun eds-org/maybe-add-filetags (key url)
  "Add some tags to the capture buffer based on the KEY for the template
being used and the URL that we're capturing."
  (message "Key: %s, URL: %s" key url)
  (when (equal key "r")
    (when-let* ((tags (cond
                      ((string-match-p "bbc.atlassian.net/browse" url) '("jira"))
                      ((string-match-p "bbc.atlassian.net/wiki" url) '("confluence"))
                      ((string-match-p "dropbox.com" url) '("docs"))
                      ((string-match-p "miro.com/app/board" url) '("miro")))))
      (save-excursion
        (goto-char (point-min))
        (vulpea-buffer-tags-add tags)))))

(provide 'eds-org)

;;; eds-org.el ends here
