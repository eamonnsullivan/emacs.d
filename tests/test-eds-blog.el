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

(load-file "tests/setup.el")

(describe "eds-blog/insert-skeleton-blog-post"
  (it "inserts Hugo front matter without an author"
    (with-temp-buffer
      (cl-letf (((symbol-function 'format-time-string)
                 (lambda (&rest _) "2026-03-19")))
        (eds-blog/insert-skeleton-blog-post "My First Blog Post")
        (expect (buffer-string)
                :to-equal
                (concat "---\n"
                        "title: \"My First Blog Post\"\n"
                        "date: \"2026-03-19T00:00:00+00:00\"\n"
                        "url: \"/posts-output/2026-03-19-my-first-blog-post/\"\n"
                        "tags: []\n"
                        "---\n\n")))))

  (it "includes a non-empty author in Hugo front matter"
    (with-temp-buffer
      (cl-letf (((symbol-function 'format-time-string)
                 (lambda (&rest _) "2026-03-19")))
        (eds-blog/insert-skeleton-blog-post "SVP News" "Paul O'Regan")
        (expect (buffer-string)
                :to-equal
                (concat "---\n"
                        "title: \"SVP News\"\n"
                        "date: \"2026-03-19T00:00:00+00:00\"\n"
                        "url: \"/posts-output/2026-03-19-svp-news/\"\n"
                        "author: \"Paul O'Regan\"\n"
                        "tags: []\n"
                        "---\n\n")))))

  (it "omits an empty author from Hugo front matter"
    (with-temp-buffer
      (cl-letf (((symbol-function 'format-time-string)
                 (lambda (&rest _) "2026-03-19")))
        (eds-blog/insert-skeleton-blog-post "Personal News" "")
        (expect (buffer-string) :not :to-match "author:")))))

(describe "eds-blog/start-blog-post"
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
             (expected-filename (concat project "/content/posts/" expected-branch ".md")))
        (expect 'find-file :to-have-been-called-with expected-filename)
        (expect 'eds-blog/insert-skeleton-blog-post :to-have-been-called-with title)
        (expect 'save-buffer :to-have-been-called)
        (expect 'magit-branch-create :to-have-been-called-with expected-branch "main")
        (expect 'magit-checkout :to-have-been-called-with expected-branch))))

  (it "passes author to the skeleton blog post"
    (let ((project "/mock/project")
          (title "My First Blog Post")
          (author "Eamonn Sullivan"))
      (spy-on 'find-file)
      (spy-on 'eds-blog/insert-skeleton-blog-post)
      (spy-on 'save-buffer)
      (spy-on 'magit-branch-create)
      (spy-on 'magit-checkout)
      (eds-blog/start-blog-post project title author)
      (expect 'eds-blog/insert-skeleton-blog-post
              :to-have-been-called-with title author))))

(describe "eds-blog/start-personal-blog-post"
  (it "passes title and author to the personal blog project"
    (spy-on 'eds-blog/start-blog-post
            :and-call-fake (lambda (&rest _) nil))
    (eds-blog/start-personal-blog-post "My Personal Post" "Eamonn Sullivan")
    (expect 'eds-blog/start-blog-post
            :to-have-been-called-with "~/git/eamonnsullivan.co.uk"
                                     "My Personal Post"
                                     "Eamonn Sullivan"))

  (it "passes nil author to the personal blog project when author is omitted"
    (spy-on 'eds-blog/start-blog-post
            :and-call-fake (lambda (&rest _) nil))
    (eds-blog/start-personal-blog-post "My Personal Post")
    (expect 'eds-blog/start-blog-post
            :to-have-been-called-with "~/git/eamonnsullivan.co.uk"
                                     "My Personal Post"
                                     nil)))

(describe "eds-blog/start-svp-blog-post"
  (it "passes title and author to the SVP blog project"
    (spy-on 'eds-blog/start-blog-post)
    (eds-blog/start-svp-blog-post "My SVP Post" "Paul O'Regan")
    (expect 'eds-blog/start-blog-post
            :to-have-been-called-with "~/git/svpsouthruislip.org.uk"
                                     "My SVP Post"
                                     "Paul O'Regan"))

  (it "passes nil author to the SVP blog project when author is omitted"
    (spy-on 'eds-blog/start-blog-post)
    (eds-blog/start-svp-blog-post "My SVP Post")
    (expect 'eds-blog/start-blog-post
            :to-have-been-called-with "~/git/svpsouthruislip.org.uk"
                                     "My SVP Post"
                                     nil)))

(describe "eds-blog/link-to-svp-contact-page"
  (it "inserts a contact link around the provided text"
    (expect (eds-blog/link-to-svp-contact-page "test")
            :to-equal "[test](/pages-output/contact/)")
    (expect (eds-blog/link-to-svp-contact-page "Hello World")
            :to-equal "[Hello World](/pages-output/contact/)"))
  (it "returns nil for empty or nil input"
    (expect (eds-blog/link-to-svp-contact-page "")
            :to-be nil)
    (expect (eds-blog/link-to-svp-contact-page nil)
            :to-be nil)))

(provide 'test-eds-blog)
;;; test-eds-blog.el ends here
