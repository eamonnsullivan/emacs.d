;;; eds-org.el --- Library of small functions and tweaks related to org mode  -*- lexical-binding:t -*-

;; Copyright (C) 2026 Eamonnn Sullivan


;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Version: 1.0
;; Keywords: emacs org-mode
;; URL: https://eamonnsullivan.co.uk
;;
;;
;;; Commentary:
;;
;;
;;; Code:

;; (require 'eds-utils)
;; (require 'eds-email)

(defun eds-org/get-org-directory ()
  "The location of my org directory varies by computer."
  (file-truename "~/Dropbox/org"))

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

(defun eds-org/create-new-note-from-clipboard-link (title)
  "Create or update an org roam node with TITLE from a (presumable) url in the clipboard."
  (interactive "sTitle: ")
  (let* ((clipboard-content (or (gui-get-selection 'CLIPBOARD) "Clipboard is empty.")))
    (org-roam-protocol-open-ref
     `(:title ,title
       :ref ,clipboard-content
       :body ""
       :template "r"))))

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

(defun eds-org/get-org-agenda-files ()
  "Return a list of org files containing the :agenda: tag, using grep and shell glob expansion."
  (let* ((org-directory (eds-org/get-org-directory))
         (default-directory (concat org-directory "/"))
         (cmd "grep -l ':agenda:' *.org")
         (output (shell-command-to-string cmd)))
    (mapcar (lambda (file) (expand-file-name file org-directory))
            (split-string output "\n" t))))

(defun eds-org/org-roam-graph-small ()
  (interactive)
  (org-roam-graph 2 (org-roam-node-at-point)))

(provide 'eds-org)

;;; eds-org.el ends here
