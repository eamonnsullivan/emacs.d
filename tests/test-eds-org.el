;;; test-eds-org.el --- unit tests for eds-org.el  -*- lexical-binding:t -*-

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

(load-file "tests/setup.el")

(describe "eds-org/create-new-note-from-clipboard-link"
  :var (gui-get-selection
        org-roam-protocol-open-ref)
  (before-all
    (fset 'gui-get-selection (lambda (type) nil))
    (fset 'org-roam-protocol-open-ref (lambda (args) nil)))

  (before-each
    (spy-on 'gui-get-selection
            :and-return-value "https://example.com")
    (spy-on 'org-roam-protocol-open-ref))

  (it "creates a new note from the clipboard link"
    (eds-org/create-new-note-from-clipboard-link "Sample Title")
    (expect 'gui-get-selection :to-have-been-called-with 'CLIPBOARD)
    (expect 'org-roam-protocol-open-ref
            :to-have-been-called-with
            '(:title "Sample Title"
              :ref "https://example.com"
              :body ""
              :template "r"))))

(describe "eds-org/set-category-value"
  :var (org-read-property-value
        org-entry-get
        org-entry-put)
  (before-all
    (fset 'org-read-property-value (lambda (property) nil))
    (fset 'org-entry-get (lambda (entry property) nil))
    (fset 'org-entry-put (lambda (entry property value) nil)))

  (before-each
    (spy-on 'org-read-property-value
            :and-return-value "Old Category")
    (spy-on 'org-entry-get
            :and-return-value "Old Category")
    (spy-on 'org-entry-put))

  (it "sets the CATEGORY property to the provided value"
    (eds-org/set-category-value "New Category")
    (expect 'org-entry-put
            :to-have-been-called-with nil "CATEGORY" "New Category"))

  (it "doesn't change the CATEGORY property if the value is the same"
    (eds-org/set-category-value "Old Category")
    (expect 'org-entry-put :not :to-have-been-called)))

(describe "eds-org/get-link-from-link"
  (it "extracts the link URL from an org link"
    (expect (eds-org/get-link-from-link "[[https://example.com][Example Site]]")
            :to-equal "https://example.com")
    (expect (eds-org/get-link-from-link "[[file:notes.org][Notes]]")
            :to-equal "file:notes.org"))
  (it "returns nil for invalid org links"
    (expect (eds-org/get-link-from-link "Not a link")
            :to-be nil)
    (expect (eds-org/get-link-from-link "[[Invalid Link]")
            :to-be nil)))

(describe "eds-org/capture-email and eds-org/capture-email-todo"
  :var (org-store-link
        eds-org/get-link-from-link
        eds-email/get-subject-from-msg
        mark-active
        region-beginning
        region-end
        buffer-substring-no-properties
        org-roam-protocol-open-ref)
  (before-all
    (fset 'org-store-link (lambda (msg arg) nil))
    (fset 'eds-org/get-link-from-link (lambda (link) nil))
    (fset 'eds-email/get-subject-from-msg (lambda (msg) nil))
    (set 'mark-active t)
    (fset 'region-beginning (lambda () 1))
    (fset 'region-end (lambda () 10))
    (fset 'org-roam-protocol-open-ref (lambda (x) nil)))
  (before-each
    (spy-on 'org-store-link
            :and-return-value "[[mail:something][RE: what about the 50K?]]")
    (spy-on 'eds-org/get-link-from-link
            :and-return-value "mail:something")
    (spy-on 'eds-email/get-subject-from-msg
            :and-return-value "RE: what about the 50K?")
    (spy-on 'buffer-substring-no-properties
            :and-return-value "This is some text from the email that has been selected.")
    (spy-on 'org-roam-protocol-open-ref))

  (it "captures a link to the  current email"
    (let ((msg '(:subject "RE: what about the 50K?")))
      (eds-org/capture-email msg)
      (expect 'org-store-link
              :to-have-been-called-with msg nil)))

  (it "captures a todo link to the  current email"
    (let ((msg '(:subject "RE: what about the 50K?")))
      (eds-org/capture-email-todo msg)
      (expect 'org-store-link
              :to-have-been-called-with msg nil)))

  (it "gets the link to the message"
    (let ((msg '(:subject "RE: what about the 50K?")))
      (eds-org/capture-email msg)
      (expect 'eds-org/get-link-from-link
              :to-have-been-called-with "[[mail:something][RE: what about the 50K?]]")))

  (it "gets the subject from the message"
    (let ((msg '(:subject "RE: what about the 50K?")))
      (eds-org/capture-email msg)
      (expect 'eds-email/get-subject-from-msg
              :to-have-been-called-with msg)))

  (it "gets the subject from the todo message"
    (let ((msg '(:subject "RE: what about the 50K?")))
      (eds-org/capture-email-todo msg)
      (expect 'eds-email/get-subject-from-msg
              :to-have-been-called-with msg)))

  (it "gets selected region contents"
    (let ((msg '(:subject "RE: what about the 50K?")))
      (eds-org/capture-email msg)
      (expect 'buffer-substring-no-properties
              :to-have-been-called)))

  (it "gets selected region contents for todo"
    (let ((msg '(:subject "RE: what about the 50K?")))
      (eds-org/capture-email-todo msg)
      (expect 'buffer-substring-no-properties
              :to-have-been-called)))

  (it "creates a new org-roam note with the email link and selected region"
    (let ((msg '(:subject "RE: what about the 50K?")))
      (eds-org/capture-email msg)
      (expect 'org-roam-protocol-open-ref
              :to-have-been-called-with
              '(:title "RE: what about the 50K?"
                :ref "mail:something"
                :body "This is some text from the email that has been selected."
                :template "r"))))

  (it "creates a new org-roam TODO with the email link and selected region"
    (let ((msg '(:subject "RE: what about the 50K?")))
      (eds-org/capture-email-todo msg)
      (expect 'org-roam-protocol-open-ref
              :to-have-been-called-with
              '(:title "RE: what about the 50K?"
                :ref "mail:something"
                :body "This is some text from the email that has been selected."
                :template "T")))))

(describe "eds-org/remove-title-boilerplate"
  (it "removes the dropbox paper suffix from the title"
    (expect (eds-org/remove-title-boilerplate "Document Title - Dropbox Paper"))
            :to-equal "Document Title")

  (it "removes the Jira suffix from the title"
    (expect (eds-org/remove-title-boilerplate "Issue Title - BBC Jira Cloud")
            :to-equal "Issue Title"))

  (it "doesn't remove the suffix if it doesn't appear at the end"
    (expect (eds-org/remove-title-boilerplate "Document Title - Dropbox Paper - Notes")
            :to-equal "Document Title - Dropbox Paper - Notes"))

  (it "removes the Confluence suffix from the title"
    (expect (eds-org/remove-title-boilerplate "Page Title - Passports - Confluence")
            :to-equal "Page Title"))

  (it "removes the Miro suffix from the title"
    (expect (eds-org/remove-title-boilerplate "Board Title - Miro")
            :to-equal "Board Title"))

  (it "removes the Richard Rohr prefix from the title"
    (expect (eds-org/remove-title-boilerplate "Richard Rohr’s Daily Meditation: Meditation Title")
            :to-equal "Meditation Title")))

(describe "eds-org/maybe-add-filetags"
  :var (vulpea-buffer-tags-add)
  (before-all
    (fset 'vulpea-buffer-tags-add (lambda (tags) nil)))

  (before-each
    (spy-on 'vulpea-buffer-tags-add))

  (it "adds 'jira' tag when the capture key is 'r' and the URL looks like a Jira ticket"
    (eds-org/maybe-add-filetags "r" "https://bbc.atlassian.net/browse/PROJ-123")
    (expect 'vulpea-buffer-tags-add :to-have-been-called-with '("jira")))

  (it "adds 'docs' tag when the capture key is 'r' and the URL looks like a Dropbox link"
    (eds-org/maybe-add-filetags "r" "https://www.dropbox.com/s/example/document")
    (expect 'vulpea-buffer-tags-add :to-have-been-called-with '("docs")))

  (it "adds 'miro' tag when the capture key is 'r' and the URL looks like a Miro link"
    (eds-org/maybe-add-filetags "r" "https://miro.com/app/board/uXjVOVtZz3I=/")
    (expect 'vulpea-buffer-tags-add :to-have-been-called-with '("miro")))

  (it "doesn't add tags when the capture key is 'r' but the URL doesn't match known patterns"
    (eds-org/maybe-add-filetags "r" "https://example.com/some-page")
    (expect 'vulpea-buffer-tags-add :not :to-have-been-called))

  (it "doesn't add tags when the capture key is not 'r'"
    (eds-org/maybe-add-filetags "x" "https://bbc.atlassian.net/browse/PROJ-123")
    (expect 'vulpea-buffer-tags-add :not :to-have-been-called)))

(describe "eds-org/sync-agenda-filetag"
  (it "adds the agenda filetag immediately after the title"
    (with-temp-buffer
      (org-mode)
      (insert "#+title: Tasks\n* TODO Do something\n")
      (eds-org/sync-agenda-filetag)
      (expect (buffer-string)
              :to-equal (concat "#+title: Tasks\n"
                                "#+filetags: :agenda:\n"
                                "* TODO Do something\n"))))

  (it "adds a missing filetags keyword after the title and before a properties drawer"
    (with-temp-buffer
      (org-mode)
      (insert "#+title: Tasks\n"
              ":PROPERTIES:\n:ID: tasks\n:END:\n"
              "* TODO Do something\n")
      (eds-org/sync-agenda-filetag)
      (expect (buffer-string)
              :to-equal (concat "#+title: Tasks\n"
                                "#+filetags: :agenda:\n"
                                ":PROPERTIES:\n:ID: tasks\n:END:\n"
                                "* TODO Do something\n"))))

  (it "adds a missing filetags keyword after a title below a properties drawer"
    (with-temp-buffer
      (org-mode)
      (insert ":PROPERTIES:\n:ID: tasks\n:END:\n"
              "#+title: Tasks\n"
              "* TODO Do something\n")
      (eds-org/sync-agenda-filetag)
      (expect (buffer-string)
              :to-equal (concat ":PROPERTIES:\n:ID: tasks\n:END:\n"
                                "#+title: Tasks\n"
                                "#+filetags: :agenda:\n"
                                "* TODO Do something\n"))))

  (it "removes the agenda filetag when all TODOs are done"
    (with-temp-buffer
      (org-mode)
      (insert "#+title: Tasks\n#+filetags: :agenda:work:\n* DONE Finished\n")
      (eds-org/sync-agenda-filetag)
      (expect (buffer-string)
              :to-match "^#\\+filetags: :work:$")
      (expect (buffer-string)
              :not :to-match ":agenda:")))

  (it "preserves other filetags when adding the agenda filetag"
    (with-temp-buffer
      (org-mode)
      (insert "#+title: Tasks\n#+filetags: :work:personal:\n* TODO Pending\n")
      (eds-org/sync-agenda-filetag)
      (expect (buffer-string)
              :to-match "^#\\+filetags: :work:personal:agenda:$")))

  (it "preserves blank lines below the filetags keyword"
    (with-temp-buffer
      (org-mode)
      (insert "#+title: Tasks\n#+filetags: :work:\n\n\n* TODO Pending\n")
      (eds-org/sync-agenda-filetag)
      (expect (buffer-string)
              :to-equal (concat "#+title: Tasks\n"
                                "#+filetags: :work:agenda:\n"
                                "\n\n* TODO Pending\n"))))

  (it "does not treat plain headings as active TODOs"
    (with-temp-buffer
      (org-mode)
      (insert "#+filetags: :agenda:\n* Notes\n")
      (eds-org/sync-agenda-filetag)
      (expect (buffer-string)
              :not :to-match "#\\+filetags:")))

  (it "ignores filetags text inside source and example blocks"
    (with-temp-buffer
      (org-mode)
      (insert "#+filetags: :agenda:\n"
              "#+begin_src org\n#+filetags: :source:\n#+end_src\n"
              "#+begin_example\n#+filetags: :example:\n#+end_example\n")
      (eds-org/sync-agenda-filetag)
      (expect (buffer-string)
              :to-match "#\\+filetags: :source:")
      (expect (buffer-string)
              :to-match "#\\+filetags: :example:")
      (expect (buffer-string)
              :not :to-match "#\\+filetags: :agenda:")))

  (it "ignores filetags keywords beneath headings"
    (with-temp-buffer
      (org-mode)
      (insert "* Notes\n#+filetags: :nested:\n* TODO Pending\n")
      (eds-org/sync-agenda-filetag)
      (expect (buffer-string)
              :to-match "^#\\+filetags: :agenda:")
      (expect (buffer-string)
              :to-match "#\\+filetags: :nested:")))

  (it "installs a buffer-local save hook"
    (with-temp-buffer
      (org-mode)
      (eds-org/enable-agenda-filetag-sync)
      (expect before-save-hook :to-contain #'eds-org/sync-agenda-filetag))))

(describe "eds-org/remove-stale-agenda-filetags"
  (it "removes agenda filetags only from files without active TODOs"
    (let* ((directory (make-temp-file "eds-org-agenda-" t))
           (stale-file (expand-file-name "stale.org" directory))
           (active-file (expand-file-name "active.org" directory)))
      (unwind-protect
          (progn
            (write-region
             "#+title: Stale\n#+filetags: :agenda:work:\n* DONE Finished\n"
             nil stale-file nil 'silent)
            (write-region
             "#+title: Active\n#+filetags: :agenda:work:\n* TODO Pending\n"
             nil active-file nil 'silent)
            (spy-on 'eds-org/get-org-agenda-files
                    :and-return-value (list stale-file active-file))
            (expect (eds-org/remove-stale-agenda-filetags)
                    :to-equal (list stale-file))
            (expect (with-temp-buffer
                      (insert-file-contents stale-file)
                      (buffer-string))
                    :to-equal
                    "#+title: Stale\n#+filetags: :work:\n* DONE Finished\n")
            (expect (with-temp-buffer
                      (insert-file-contents active-file)
                      (buffer-string))
                    :to-equal
                    "#+title: Active\n#+filetags: :agenda:work:\n* TODO Pending\n"))
        (delete-directory directory t))))

  (it "leaves an existing file buffer open"
    (let* ((file (make-temp-file "eds-org-agenda-" nil ".org"
                                 "#+filetags: :agenda:\n* DONE Finished\n"))
           (buffer (find-file-noselect file)))
      (unwind-protect
          (progn
            (spy-on 'eds-org/get-org-agenda-files
                    :and-return-value (list file))
            (eds-org/remove-stale-agenda-filetags)
            (expect (buffer-live-p buffer) :to-be-truthy))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))
        (delete-file file)))))

(provide 'test-eds-org)
;;; test-eds-org.el ends here
