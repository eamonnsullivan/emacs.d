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

(describe "eds-org category helpers"
  (before-all
    (require 'org))

  (before-each
    (spy-on 'completing-read :and-return-value "Work"))

  (it "prompts despite a filename-derived category and writes a file property"
    (with-temp-buffer
      (org-mode)
      (setq buffer-file-name "/tmp/20260326-example.org")
      (insert ":PROPERTIES:\n:ID: example\n:END:\n#+title: Example\n")
      (goto-char (point-min))
      (expect (org-entry-get nil "CATEGORY") :to-equal "20260326-example")
      (expect (eds-org/explicit-category) :to-be nil)
      (eds-org/ensure-capture-category)
      (expect 'completing-read :to-have-been-called)
      (expect (eds-org/explicit-category) :to-equal "Work")
      (expect (point) :to-equal (point-min))))

  (it "keeps an explicit category even when it equals the filename"
    (with-temp-buffer
      (org-mode)
      (setq buffer-file-name "/tmp/Work.org")
      (insert ":PROPERTIES:\n:CATEGORY: Work\n:END:\n#+title: Example\n")
      (goto-char (point-min))
      (eds-org/ensure-capture-category)
      (expect 'completing-read :not :to-have-been-called)))

  (it "keeps a file category keyword"
    (with-temp-buffer
      (org-mode)
      (insert "#+category: Meeting\n* Notes\n")
      (goto-char (point-max))
      (expect (eds-org/explicit-category) :to-equal "Meeting")
      (eds-org/ensure-capture-category)
      (expect 'completing-read :not :to-have-been-called)))

  (it "keeps inherited file and parent categories"
    (dolist (text '(":PROPERTIES:\n:CATEGORY: Note\n:END:\n* Child\n"
                    "* Parent\n:PROPERTIES:\n:CATEGORY: Note\n:END:\n** Child\n"))
      (with-temp-buffer
        (org-mode)
        (insert text)
        (goto-char (point-max))
        (expect (eds-org/explicit-category) :to-equal "Note")
        (eds-org/ensure-capture-category)))
    (expect 'completing-read :not :to-have-been-called))

  (it "prefers a local category over an ancestor category"
    (with-temp-buffer
      (org-mode)
      (insert ":PROPERTIES:\n:CATEGORY: Note\n:END:\n* Child\n:PROPERTIES:\n:CATEGORY: Work\n:END:\n")
      (goto-char (point-max))
      (expect (eds-org/explicit-category) :to-equal "Work")))

  (it "prompts for an empty explicit category"
    (with-temp-buffer
      (org-mode)
      (insert ":PROPERTIES:\n:CATEGORY:\n:END:\n#+title: Example\n")
      (goto-char (point-min))
      (eds-org/ensure-capture-category)
      (expect (eds-org/explicit-category) :to-equal "Work")))

  (it "writes a heading category and preserves point and narrowing"
    (with-temp-buffer
      (org-mode)
      (insert "#+title: Example\n* Child\nBody\n")
      (search-backward "* Child")
      (org-narrow-to-subtree)
      (let ((position (point))
            (start (point-min)))
        (eds-org/ensure-capture-category)
        (expect (point) :to-equal position)
        (expect (point-min) :to-equal start)
        (expect (buffer-narrowed-p) :to-be-truthy)
        (expect (eds-org/explicit-category) :to-equal "Work")
        (widen)
        (goto-char (point-min))
        (expect (eds-org/explicit-category) :to-be nil))))

  (it "offers existing and previous categories while allowing new values"
    (with-temp-buffer
      (org-mode)
      (insert "* Other\n:PROPERTIES:\n:CATEGORY: Meeting\n:END:\n")
      (let ((eds-org/category-history '("Note")))
        (expect (eds-org/read-category) :to-equal "Work")
        (expect 'completing-read :to-have-been-called-with
                "Category: " '("Meeting" "Note") nil nil nil
                'eds-org/category-history))))

  (it "re-prompts after blank input and trims the answer"
    (spy-on 'completing-read :and-call-fake
            (let ((answers '("   " " Work ")))
              (lambda (&rest _args) (pop answers))))
    (with-temp-buffer
      (org-mode)
      (expect (eds-org/read-category) :to-equal "Work")
      (expect (length (spy-calls-all 'completing-read)) :to-equal 2)))

  (it "writes an explicit manual category even when equal to the filename"
    (with-temp-buffer
      (org-mode)
      (setq buffer-file-name "/tmp/Work.org")
      (insert "#+title: Example\n* Child\n")
      (goto-char (point-max))
      (eds-org/set-category-value "Work")
      (goto-char (point-min))
      (expect (eds-org/explicit-category) :to-equal "Work")
      (expect 'completing-read :not :to-have-been-called)))

  (it "prompts when setting a category manually without a value"
    (with-temp-buffer
      (org-mode)
      (eds-org/set-category-value nil)
      (expect (eds-org/explicit-category) :to-equal "Work"))))

(describe "eds-org/get-link-from-link"
  (it "extracts the bracketed link from an org link"
    (expect (eds-org/get-link-from-link "[[https://example.com][Example Site]]")
            :to-equal "[[https://example.com][Example Site]]")
    (expect (eds-org/get-link-from-link "[[file:notes.org][Notes]]")
            :to-equal "[[file:notes.org][Notes]]"))
  (it "keeps the brackets so org-roam accepts the ref"
    (expect (eds-org/get-link-from-link "[[mu4e:msgid:abc@example.com][RE: hello]]")
            :to-equal "[[mu4e:msgid:abc@example.com][RE: hello]]"))
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
            :and-return-value "[[mail:something][RE: what about the 50K?]]")
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
                :ref "[[mail:something][RE: what about the 50K?]]"
                :body "This is some text from the email that has been selected."
                :template "r"))))

  (it "creates a new org-roam TODO with the email link and selected region"
    (let ((msg '(:subject "RE: what about the 50K?")))
      (eds-org/capture-email-todo msg)
      (expect 'org-roam-protocol-open-ref
              :to-have-been-called-with
              '(:title "RE: what about the 50K?"
                :ref "[[mail:something][RE: what about the 50K?]]"
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

(provide 'test-eds-org)
;;; test-eds-org.el ends here
