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

;;; Code:

(require 'seq)

;;;###autoload
(defun eds-org/get-org-directory ()
  "The location of my org directory varies by computer."
  (file-truename "~/Dropbox/org"))

;;;###autoload
(defun eds-org/get-conflicted-org-files ()
  "Search the org directory for files with \"conflicted\" in the name."
  (eds-utils/filter-for-regex "conflicted" (directory-files (eds-org/get-org-directory))))

(defun eds-org/get-org-file-name (title)
  "Get a suitable org-roam filename for TITLE."
  (let* ((time-string (eds-utils/get-time-string))
         (extension "org")
         (clean-title (eds-utils/strip-invalid-chars title))
         (slug (eds-utils/process-title clean-title))
         (file-base-name (concat time-string "-" slug "." extension)))
    (expand-file-name file-base-name (eds-org/get-org-directory))))

(defvar eds-org/category-history nil
  "History of categories entered for CATEGORY properties.")

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
(defun eds-org/org-roam-graph-small ()
  (interactive)
  (org-roam-graph 2 (org-roam-node-at-point)))

(provide 'eds-org)

;;; eds-org.el ends here
