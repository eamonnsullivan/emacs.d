;;; eds-blog.el --- Library of small functions related to composing blogs  -*- lexical-binding:t -*-
;;
;; Copyright (C) 2026 Eamonnn Sullivan
;;
;;
;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Version: 1.0
;; Keywords: emacs blogs
;; URL: https://eamonnsullivan.co.uk
;;
;;
;;; Commentary:
;;
;;
;;; Code:

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

(defun eds-blog/start-personal-blog-post (title)
  "Create a new post on my personal blog with TITLE."
  (interactive "sTitle: ")
  (eds-blog/start-blog-post "~/git/eamonnsullivan.co.uk" title))

(defun eds-blog/start-svp-blog-post (title)
  "Create a new post on the svp blog with TITLE."
  (interactive "sTitle: ")
  (eds-blog/start-blog-post "~/git/svpsouthruislip.org.uk" title))

(defun eds-blog/link-to-svp-contact-page (selected-text)
  "Make a link to the SVP's contact page from SELECTED-TEXT."
  (if (> (length selected-text) 0)
      (format "[%s](../../pages-output/contact/)" selected-text)
    nil))

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
