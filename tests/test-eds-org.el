;;; test-eds-org.el --- Unit tests for eds-org.el -*- lexical-binding: t; -*-

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
(require 'org)

(describe "eds-org category helpers"
  (before-each
    (spy-on 'completing-read :and-return-value "Work"))

  (it "prefers a local category over an ancestor category"
    (with-temp-buffer
      (org-mode)
      (insert ":PROPERTIES:\n:CATEGORY: Note\n:END:\n* Child\n:PROPERTIES:\n:CATEGORY: Work\n:END:\n")
      (goto-char (point-max))
      (expect (eds-org/explicit-category) :to-equal "Work")))

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

(provide 'test-eds-org)
;;; test-eds-org.el ends here
