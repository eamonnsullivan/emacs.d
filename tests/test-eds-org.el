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

(require 'eds-org)

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
    (fset 'buffer-substring-no-properties (lambda (beg end) nil))
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

  (it "captures a link to the  current email"
    (let ((msg '(:subject "RE: what about the 50K?")))
      (eds-org/capture-email-todo msg)
      (expect 'org-store-link
              :to-have-been-called-with msg nil)))

  (it "gets the link to the message"
    (let ((msg '(:subject "RE: what about the 50K?")))
      (eds-org/capture-email msg)
      (expect 'eds-org/get-link-from-link
              :to-have-been-called-with "[[mail:something][RE: what about the 50K?]]")))

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

  (it "gets the subject from the message"
    (let ((msg '(:subject "RE: what about the 50K?")))
      (eds-org/capture-email-todo msg)
      (expect 'eds-email/get-subject-from-msg
              :to-have-been-called-with msg)))

  (it "gets selected region contents"
    (let ((msg '(:subject "RE: what about the 50K?")))
      (eds-org/capture-email msg)
      (expect 'buffer-substring-no-properties
              :to-have-been-called)))

  (it "gets selected region contents"
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

(provide 'test-eds-org)
;;; test-eds-org.el ends here
