;;; test-eds-blog.el --- unit tests for eds-blog.el  -*- lexical-binding:t -*-
;;
;; Copyright (C) 2026 Eamonnn Sullivan
;;
;;
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

(describe "eds-blog/start-blog-post"
  :var (find-file
        eds-blog/insert-skeleton-blog-post
        save-buffer
        magit-branch-create
        magit-checkout)
  (before-all
    (fset 'find-file (lambda (filename) nil))
    (fset 'eds-blog/insert-skeleton-blog-post (lambda (title) nil))
    (fset 'save-buffer (lambda () nil))
    (fset 'magit-branch-create (lambda (branch base) nil))
    (fset 'magit-checkout (lambda (branch) nil)))

  (it "creates a new blog post file with the correct name and branch"
    (let ((project "/mock/project")
          (title "My First Blog Post"))
      (spy-on 'find-file)
      (spy-on 'eds-blog/insert-skeleton-blog-post)
      (spy-on 'save-buffer)
      (spy-on 'magit-branch-create)
      (spy-on 'magit-checkout)
      (eds-blog/start-blog-post project title)
      (let* ((expected-branch (concat (format-time-string "%Y-%m-%d") "-my-first-blog-post"))
             (expected-filename (concat project "/content/md/posts/" expected-branch ".md")))
        (expect 'find-file :to-have-been-called-with expected-filename)
        (expect 'eds-blog/insert-skeleton-blog-post :to-have-been-called-with title)
        (expect 'save-buffer :to-have-been-called)
        (expect 'magit-branch-create :to-have-been-called-with expected-branch "main")
        (expect 'magit-checkout :to-have-been-called-with expected-branch)))))

(describe "eds-blog/link-to-svp-contact-page"
  (it "inserts a contact link around the provided text"
    (expect (eds-blog/link-to-svp-contact-page "test")
            :to-equal "[test](../../pages-output/contact/)")
    (expect (eds-blog/link-to-svp-contact-page "Hello World")
            :to-equal "[Hello World](../../pages-output/contact/)"))
  (it "returns nil for empty or nil input"
    (expect (eds-blog/link-to-svp-contact-page "")
            :to-be nil)
    (expect (eds-blog/link-to-svp-contact-page nil)
            :to-be nil)))

(provide 'test-eds-blog)
;;; test-eds-blog.el ends here
