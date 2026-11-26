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
;; Package initialization reports readiness; this module registers capture policy.

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
(require 'eds-org)
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

(defvar eds-org-capture--ready-facts nil
  "Capture readiness facts reported so far.
`org', `org-roam' and `vulpea' mean their libraries have loaded.
`vulpea-configured' means Vulpea setup completed successfully.")

(defvar eds-org-index-file nil
  "Org file that receives index notes.  Set by `init-org'.")

(defvar eds-org-personal-file nil
  "Org file that receives personal notes.  Set by `init-org'.")

(defun eds-org-capture--ready-p (fact)
  "Return non-nil when capture readiness FACT has been reported."
  (memq fact eds-org-capture--ready-facts))

(defun eds-org-capture--register-org-templates ()
  "Set the Org capture menu from the reported readiness facts.
File targets resolve at capture time, so the menu may be built before
the configuration sets `eds-org-index-file' and `eds-org-personal-file'."
  (setopt org-capture-templates
          `(,@(when (eds-org-capture--ready-p 'vulpea-configured)
                (list (eds-org-capture--meeting-template)))
            ("t" "Todo" entry (file ,(lambda () eds-org-index-file))
             "* TODO %?\n SCHEDULED: %t\n %a")
            ("w" "Work note" entry (file ,(lambda () org-default-notes-file))
             "* %?\n %U\n %a")
            ("p" "Personal note" entry (file ,(lambda () eds-org-personal-file))
             "* %?\n %U\n %a"))))

(defun eds-org-capture--register-roam-templates ()
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

(defun eds-org-capture--register-vulpea-defaults ()
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

(defun eds-org-capture--meeting-template ()
  "Return the Org capture template for a Vulpea meeting note."
  '("m" "meeting" entry
    (file (lambda ()
            (vulpea-note-path
             (vulpea-create
              "Meeting notes"
              nil
              :tags '("meeting")
              :properties (list (cons "CREATED" (format-time-string "%FT%T%z"))
                                (cons "CATEGORY" "Meeting"))))))
    "* Actions\n* Notes\n%u\n- [[id:6D43870C-DBA0-4E2D-88D9-3D25BB693FD9][meetings]]\n%?"))

(defun eds-org-capture-notify-ready (fact)
  "Update capture policy affected by a newly reported readiness FACT.

FACT is `org', `org-roam', `vulpea' or `vulpea-configured'.  Facts may
arrive in any order, and reporting one twice changes nothing."
  (unless (eds-org-capture--ready-p fact)
    (pcase fact
      ('org (eds-org-capture--register-org-templates))
      ('org-roam (eds-org-capture--register-roam-templates))
      ('vulpea (eds-org-capture--register-vulpea-defaults))
      ('vulpea-configured
       (when (eds-org-capture--ready-p 'org)
         (setopt org-capture-templates
                 (cons (eds-org-capture--meeting-template)
                       (seq-remove (lambda (template) (equal (car template) "m"))
                                   org-capture-templates)))))
      (_ (error "Unknown capture readiness fact: %S" fact)))
    (push fact eds-org-capture--ready-facts)))

(defun eds-org-capture--listen (fact feature)
  "Record capture readiness FACT now, or when FEATURE finishes loading."
  (if (featurep feature)
      (eds-org-capture-notify-ready fact)
    (with-eval-after-load feature
      (eds-org-capture-notify-ready fact))))

(eds-org-capture--listen 'org 'org)
(eds-org-capture--listen 'org-roam 'org-roam)
(eds-org-capture--listen 'vulpea 'vulpea)

(provide 'eds-org-capture)
;;; eds-org-capture.el ends here
