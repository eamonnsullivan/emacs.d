;;; eds-org-agenda.el --- Maintain Org agenda eligibility -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan <me@eamonnsullivan.co.uk>

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Maintainer: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Created: 2026-09-24
;; Version: 0.1
;; Package-Requires: ((emacs "31.0"))
;; Keywords: convenience, outlines
;; URL: https://github.com/eamonnsullivan/eds-utils

;;; Commentary:

;; Maintain agenda eligibility, its derived file marker, and agenda discovery.

;;; Code:

(require 'org)
(require 'org-element)
(require 'seq)
(require 'subr-x)
(require 'eds-org)

(defgroup eds-org-agenda nil
  "Maintain Org agenda eligibility."
  :group 'org-agenda)

(defcustom eds-org-agenda-explicit-files '("calendar.org")
  "Files always eligible for agenda inclusion.
Relative paths are resolved beneath `eds-org/get-org-directory'."
  :type '(repeat file)
  :group 'eds-org-agenda)

(defun eds-org-agenda--active-work-p ()
  "Return non-nil when current Org buffer contains active work."
  (org-element-map (org-element-parse-buffer) 'headline
    (lambda (headline)
      (member (org-element-property :todo-keyword headline)
              org-not-done-keywords))
    nil t))

(defun eds-org-agenda--explicit-paths ()
  "Return absolute paths configured as explicit agenda files."
  (mapcar (lambda (file)
            (expand-file-name file (eds-org/get-org-directory)))
          eds-org-agenda-explicit-files))

(defun eds-org-agenda--eligible-p ()
  "Return non-nil when current buffer is eligible for agenda inclusion."
  (or (eds-org-agenda--active-work-p)
      (and buffer-file-name
           (member (expand-file-name buffer-file-name)
                   (eds-org-agenda--explicit-paths)))))

(defun eds-org-agenda--top-level-filetags ()
  "Return top-level FILETAGS keyword elements in current Org buffer."
  (let* ((document (org-element-parse-buffer))
         (first-element (car (org-element-contents document))))
    (when (eq (org-element-type first-element) 'section)
      (seq-filter
       (lambda (element)
         (and (eq (org-element-type element) 'keyword)
              (string-equal (org-element-property :key element) "FILETAGS")))
       (org-element-contents first-element)))))

(defun eds-org-agenda--add-marker (keyword)
  "Add agenda marker to FILETAGS KEYWORD, preserving other text."
  (let* ((begin (org-element-property :begin keyword))
         (value (org-element-property :value keyword))
         (tags (split-string value "[ :\t]+" t)))
    (unless (member "agenda" tags)
      (goto-char begin)
      (when (search-forward value (line-end-position) t)
        (if (string-suffix-p ":" value)
            (progn
              (backward-char)
              (insert ":agenda"))
          (insert (if (string-empty-p value) ":agenda:" ":agenda")))))))

(defun eds-org-agenda--remove-marker (keyword)
  "Remove agenda marker from FILETAGS KEYWORD, preserving other content."
  (let* ((begin (org-element-property :begin keyword))
         (value (org-element-property :value keyword))
         (without-marker
          (replace-regexp-in-string
           "\\(?:\\_<agenda\\_>:\\|:\\_<agenda\\_>\\)" "" value)))
    (unless (string-equal value without-marker)
      (goto-char begin)
      (when (search-forward value (line-end-position) t)
        (replace-match without-marker t t)))))

(defun eds-org-agenda--marker-insertion-position ()
  "Return position immediately after current buffer's top-level title."
  (let* ((document (org-element-parse-buffer))
         (first-element (car (org-element-contents document)))
         (title
          (when (eq (org-element-type first-element) 'section)
            (seq-find
             (lambda (element)
               (and (eq (org-element-type element) 'keyword)
                    (string-equal (org-element-property :key element) "TITLE")))
             (org-element-contents first-element)))))
    (if title (org-element-property :end title) (point-min))))

(defun eds-org-agenda--sync-marker ()
  "Synchronize agenda marker with agenda eligibility in current buffer."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (save-restriction
        (widen)
        (let ((keywords (eds-org-agenda--top-level-filetags)))
          (if (eds-org-agenda--eligible-p)
              (if-let* ((keyword (car keywords)))
                  (eds-org-agenda--add-marker keyword)
                (goto-char (eds-org-agenda--marker-insertion-position))
                (insert "#+filetags: :agenda:\n"))
            (dolist (keyword (reverse keywords))
              (eds-org-agenda--remove-marker keyword))))))))

;;;###autoload
(defun eds-org-agenda-enable-sync ()
  "Synchronize agenda marker before saving current Org buffer."
  (add-hook 'before-save-hook #'eds-org-agenda--sync-marker nil t))

;;;###autoload
(defun eds-org-agenda-refresh (&rest _)
  "Refresh `org-agenda-files' from agenda markers and explicit files."
  (interactive)
  (setq org-agenda-files
        (seq-uniq
         (append
          (mapcar #'vulpea-note-path
                  (vulpea-db-query-by-tags-some '("agenda")))
          (eds-org-agenda--explicit-paths))))
  (when (called-interactively-p 'interactive)
    (message "Updated org-agenda-files: %d" (length org-agenda-files)))
  org-agenda-files)

(defun eds-org-agenda--repair-file (file apply)
  "Repair agenda marker in FILE when APPLY is non-nil.
Return non-nil when marker needs changing."
  (with-temp-buffer
    (insert-file-contents file)
    (setq buffer-file-name file)
    (org-mode)
    (let ((original (buffer-string)))
      (eds-org-agenda--sync-marker)
      (unless (string-equal original (buffer-string))
        (when apply
          (write-region (point-min) (point-max) file nil 'silent))
        t))))

;;;###autoload
(defun eds-org-agenda-repair (&optional apply)
  "Repair agenda markers recursively beneath Org directory.
Without APPLY, report needed changes without writing.  Interactively, a
prefix argument enables APPLY.  Modified visiting buffers are skipped.
Return plist with `:changed', `:would-change', `:skipped', and `:failed'."
  (interactive "P")
  (let ((files (directory-files-recursively
                (eds-org/get-org-directory) "\\.org\\'"))
        changed would-change skipped failed)
    (dolist (file files)
      (let ((buffer (find-buffer-visiting file)))
        (if (and buffer (buffer-modified-p buffer))
            (push file skipped)
          (condition-case error-data
              (when (eds-org-agenda--repair-file file apply)
                (push file (if apply changed would-change)))
            (error (push (cons file (error-message-string error-data))
                         failed))))))
    (let ((result (list :changed (nreverse changed)
                        :would-change (nreverse would-change)
                        :skipped (nreverse skipped)
                        :failed (nreverse failed))))
      (when (called-interactively-p 'interactive)
        (message "Agenda repair: %d changed, %d would change, %d skipped, %d failed"
                 (length (plist-get result :changed))
                 (length (plist-get result :would-change))
                 (length (plist-get result :skipped))
                 (length (plist-get result :failed))))
      result)))

(provide 'eds-org-agenda)

;;; eds-org-agenda.el ends here
