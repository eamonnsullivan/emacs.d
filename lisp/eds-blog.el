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

(require 'eds-utils)

(defun eds-blog/insert-skeleton-blog-post (title)
  "Insert a basic skeleton for a blog post with TITLE."
  (goto-char (point-min))
  (insert (format "{:title \"%s\"\n :layout :post\n :tags []}\n\n" title)))

(defun eds-blog/start-blog-post (project title)
  "Create a new post for PROJECT with TITLE ande today's date on a new branch."
  (let* ((title-name (eds-utils/process-title title))
         (branch (concat (format-time-string "%Y-%m-%d") "-" title-name))
         (filename (concat project "/content/md/posts/" branch ".md")))
    (find-file filename)
    (eds-blog/insert-skeleton-blog-post title)
    (save-buffer)
    (magit-branch-create branch "main")
    (magit-checkout branch)))

;;;###autoload
(defun eds-blog/start-personal-blog-post (title)
  "Create a new post on my personal blog with TITLE."
  (interactive "sTitle: ")
  (eds-blog/start-blog-post "~/git/eamonnsullivan.co.uk" title))

;;;###autoload
(defun eds-blog/start-svp-blog-post (title)
  "Create a new post on the svp blog with TITLE."
  (interactive "sTitle: ")
  (eds-blog/start-blog-post "~/git/svpsouthruislip.org.uk" title))

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
