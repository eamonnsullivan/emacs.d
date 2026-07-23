;;; eds-blog.el --- Blogging tools and initialisation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan <me@eamonnsullivan.co.uk>

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Maintainer: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Version: 0.1
;; Package-Requires: ((emacs "31.0"))
;; Keywords: blog, publishing, tools
;; URL: https://github.com/eamonnsullivan/eds-blog

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides initialisation and tools for blogging workflows
;; in Emacs, supporting publishing, editing, and organisation.

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

(require 'eds-utils)
(require 'subr-x)

(defun eds-blog/insert-skeleton-blog-post (title &optional author)
  "Insert a basic skeleton for a blog post with TITLE.
When AUTHOR is non-nil and non-empty, include it in the post metadata."
  (goto-char (point-min))
  (insert (format "{:title \"%s\"\n :layout :post\n%s :tags []}\n\n"
                  title
                  (if (and author (not (string-empty-p author)))
                      (format " :author \"%s\"\n" author)
                    ""))))

(defun eds-blog/start-blog-post (project title &optional author)
  "Create a new post for PROJECT with TITLE and today's date on a new branch.
When AUTHOR is non-nil and non-empty, include it in the post metadata."
  (let* ((title-name (eds-utils/process-title title))
         (branch (concat (format-time-string "%Y-%m-%d") "-" title-name))
         (filename (concat project "/content/md/posts/" branch ".md"))
         (default-directory (file-name-as-directory (expand-file-name project))))
    (magit-branch-create branch "main")
    (magit-checkout branch)
    (find-file filename)
    (if (and author (not (string-empty-p author)))
        (eds-blog/insert-skeleton-blog-post title author)
      (eds-blog/insert-skeleton-blog-post title))
    (save-buffer)))

(defun eds/launch-blog-ux ()
  "Open a panel to start a blog post."
  (interactive)
  (let* ((buffer (get-buffer-create "*Blog*"))
         (window (display-buffer-in-side-window buffer '((side . bottom)
                                                         (slot . 0)
                                                         (window-height . 12)))))
    (select-window window)
    (kill-all-local-variables)
    (let ((inhibit-read-only t))
      (erase-buffer)))
  (remove-overlays)
  (widget-insert "Start a blog post:\n\n")
  (let* ((project (widget-create 'radio-button-choice
                                 :tag "Project: "
                                 :value "~/git/svpsouthruislip.org.uk"
                                 '(item :tag "SVP" "~/git/svpsouthruislip.org.uk")
                                 '(item :tag "Personal" "~/git/eamonnsullivan.co.uk")))
         (title (widget-create 'editable-field
                               :format "Title: %v\n"
                               ""))
         (author (widget-create 'editable-field
                                :format "Author: %v\n"
                                "Paul O'Regan")))
    (widget-put project
                :notify
                (lambda (widget &rest _)
                  (pcase (widget-value widget)
                    ("~/git/svpsouthruislip.org.uk"
                     (widget-apply author :value-set "Paul O'Regan"))
                    ("~/git/eamonnsullivan.co.uk"
                     (widget-apply author :value-set "Eamonn Sullivan")))))

    (widget-create 'push-button
                   :notify (lambda (&rest _) (kill-buffer))
                   "Cancel")
    (widget-insert " ")
    (widget-create 'push-button
                   :notify (lambda (&rest _)
                             (let ((project-value (widget-value project))
                                   (title-value (widget-value title))
                                   (author-value (widget-value author)))
                               (kill-buffer)
                               (eds-blog/start-blog-post project-value title-value author-value)))
                   "Create")
    (use-local-map widget-keymap)
    (widget-setup)
    (goto-char (widget-get title :from))
    (widget-forward 1)))

;;;###autoload
(defun eds-blog/start-personal-blog-post (title &optional author)
  "Create a new post on my personal blog with TITLE.
When AUTHOR is non-nil and non-empty, include it in the post metadata."
  (interactive "sTitle: ")
  (eds-blog/start-blog-post "~/git/eamonnsullivan.co.uk" title author))

;;;###autoload
(defun eds-blog/start-svp-blog-post (title &optional author)
  "Create a new post on the svp blog with TITLE.
When AUTHOR is non-nil and non-empty, include it in the post metadata."
  (interactive "sTitle: ")
  (eds-blog/start-blog-post "~/git/svpsouthruislip.org.uk" title author))

(defun eds-blog/link-to-svp-contact-page (selected-text)
  "Make a link to the SVP's contact page from SELECTED-TEXT."
  (if (> (length selected-text) 0)
      (format "[%s](../../pages-output/contact/)" selected-text)
    nil))

;;;###autoload
(defun eds-blog/make-svp-contact-link ()
  "Make a link to the SVP's contact page from the current selection."
  (interactive)
  (if (use-region-p)
      (let ((yank-text (buffer-substring-no-properties (region-beginning) (region-end))))
        (if (> (length yank-text) 0)
            (progn
              (delete-region (region-beginning) (region-end))
              (insert (eds-blog/link-to-svp-contact-page yank-text)))
          (message "No active selection found!")))))


(provide 'eds-blog)
;;; eds-blog.el ends here
