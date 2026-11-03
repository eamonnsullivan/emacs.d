;;; eds-org-capture.el --- Capture policy for Org workflows -*- lexical-binding: t; -*-

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

;; Own capture templates, CATEGORY policy and reference metadata.
;; Package initialization calls registration helpers after each package loads.

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

(require 'eds-email)
(require 'seq)
(require 'subr-x)

(defun eds-org/get-link-from-link (link)
  "Extract the URL from an org-roam LINK.

The brackets are kept, because org-roam only accepts `ROAM_REFS'
values that match `org-link-any-re'.  A bare `mu4e:msgid:...' string
does not match, while the bracketed form does."
  (if (string-match "\\[\\[\\(.*?\\)\\]\\[.*?\\]\\]" link)
      (match-string 0 link)
    nil))

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

(defun eds-org--capture-email (msg template)
  "Capture email MSG into org-roam using reference TEMPLATE.
Preserve the bracketed reference and include selected text when the mark
is active; otherwise use an empty body."
  (let* ((link (org-store-link msg nil))
         (ref (eds-org/get-link-from-link link))
         (subject (eds-email/get-subject-from-msg msg))
         (body (if mark-active
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 "")))
    (org-roam-protocol-open-ref
     `(:title ,subject
       :ref ,ref
       :body ,body
       :template ,template))))

;;;###autoload
(defun eds-org/capture-email (msg)
  "Capture an email message MSG into org-roam."
  (interactive)
  (eds-org--capture-email msg "r"))

;;;###autoload
(defun eds-org/capture-email-todo (msg)
  "Capture an email message MSG into a TODO in org-roam."
  (interactive)
  (eds-org--capture-email msg "T"))

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
  "Add capture tags based on template KEY and captured URL."
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

(defun eds-org-capture-before-finalize ()
  "Apply reference tags to the current capture before finalization."
  (eds-org/maybe-add-filetags (plist-get org-capture-plist :key)
                              (plist-get org-capture-plist :annotation)))

(defun eds-org-capture-register-org-templates ()
  "Register ordinary Org capture templates after Org configuration."
  (setopt org-capture-templates `(("t" "Todo" entry (file eds-org-index-file)
                                   "* TODO %?\n SCHEDULED: %t\n %a")
                                  ("w" "Work note" entry (file org-default-notes-file)
                                   "* %?\n %U\n %a")
                                  ("p" "Personal note" entry (file eds-org-personal-file)
                                   "* %?\n %U\n %a"))))

(defun eds-org-capture-register-roam-templates ()
  "Register Org-roam capture policy after Org-roam loads."
  (add-hook 'org-roam-capture-new-node-hook #'eds-org/ensure-capture-category)
  (add-hook 'org-capture-before-finalize-hook #'eds-org-capture-before-finalize)
  (setopt org-roam-capture-templates
        `(("d" "default" entry "* ${title}\n%?"
           :target (file+head
                    "%<%Y%m%d%H%M%S>-${slug}.org"
                    ,(concat "#+title: ${title}"
                             "\n#+startup: content"))
           :unnarrowed t)
          ("m" "meeting" entry "* ${title} Notes\n%u\n- [[id:6D43870C-DBA0-4E2D-88D9-3D25BB693FD9][meetings]]\n%?"
           :target
           (file+head
            "%<%Y%m%d%H%M%S>-${slug}.org"
            ,(concat "#+title: ${title}"
                     "\n#+filetags: :meeting:"
                     "\n#+startup: content"
                     "\n* Actions\n"))
           :unnarrowed t)
          ("t" "todo" entry "* TODO %?\n%t\n"
           :target (file+head
                    "%<%Y%m%d%H%M%S>-${slug}.org"
                    ,(concat "#+title: ${title}"
                             "\n#+startup: content"))
           :unnarrowed t)
          ("x" "training" entry "* ${title} Notes\n%u\n%?"
           :target
           (file+head
            "%<%Y%m%d%H%M%S>-${slug}.org"
            ,(concat "#+title: ${title}"
                     "\n#+filetags: :training:"
                     "\n#+startup: content"
                     "\n- [[id:C9EB836D-F0DD-47CE-B8F3-B44FD1A0A0E6][training]]\n"))
           :unnarrowed t)
          ("i" "idea" entry "* %?\n- [[id:A5284C15-BADD-4A2D-8299-6A8A24339000][Ideas]]\n"
           :target (file+head
                    "%<%Y%m%d%H%M%S>-${slug}.org"
                    ,(concat "#+title: ${title}"
                             "\n#+filetags: :ideas:"
                             "\n#+startup: content"))))
        org-roam-capture-ref-templates
        `(("r" "ref" plain "%?"
           :target
           (file+head
            "%<%Y%m%d%H%M%S>-${slug}.org"
            "%(concat \"#+title: \"
                      (eds-org/remove-title-boilerplate \"${title}\")
                      \"\n#+filetags: :reference:\"
                      \"\n#+startup: content\n\"
                      \"\n${body}\n\")")
           :unnarrowed t)
          ("T" "ref" entry "* TODO %?\n%t\n\n${body}\n"
           :target
           (file+head
            "%<%Y%m%d%H%M%S>-${slug}.org"
            ,(concat "#+title: ${title}"
                     "\n#+startup: content"))
           :unnarrowed t))))

(defun eds-org-capture-register-vulpea-defaults ()
  "Register Vulpea note defaults after Vulpea loads."
  (setopt vulpea-create-default-function
          (lambda (title)
            (list :file-name (format "%s_%s.org"
                                     (format-time-string "%Y%m%d%H%M%S")
                                     (vulpea-title-to-slug title))
                  :tags (when (string-match-p "meeting" title)
                            '("meeting"))
                  :properties (if (string-match-p "meeting" title)
                                  (list (cons "CREATED" (format-time-string "%FT%T%z"))
                                        (cons "CATEGORY" "Meeting"))
                                (list (cons "CREATED" (format-time-string "%FT%T%z"))
                                      (cons "CATEGORY" "Note")))
                  :body (if (string-match-p "meeting" title)
                            "\n\n* Actions\n* Notes\n- [[id:6D43870C-DBA0-4E2D-88D9-3D25BB693FD9][meetings]]\n"
                          "\n")))))

(defun eds-org-capture-register-vulpea-meeting-template ()
  "Register Vulpea meeting capture after autosync setup."
  (add-to-list 'org-capture-templates
               '("m" "meeting" entry
                 (file (lambda ()
                         (vulpea-note-path
                          (vulpea-create
                           "Meeting notes"
                           nil
                           :tags '("meeting")
                           :properties (list (cons "CREATED" (format-time-string "%FT%T%z"))
                                             (cons "CATEGORY" "Meeting"))))))
                 "* Actions\n* Notes\n%u\n- [[id:6D43870C-DBA0-4E2D-88D9-3D25BB693FD9][meetings]]\n%?")))

(provide 'eds-org-capture)
;;; eds-org-capture.el ends here
