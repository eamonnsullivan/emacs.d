;;; test-eds-org-agenda.el --- Tests for agenda eligibility -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Version: 0.1
;; Keywords: emacs
;; URL: https://eamonnsullivan.co.uk

;;; Commentary:

;; Behavior tests for agenda eligibility and its derived marker.

;;; Code:

(load-file "tests/setup.el")
(require 'eds-org-agenda)

(describe "eds-org-agenda-enable-sync"
  (it "synchronizes the agenda marker before save without merging other FILETAGS"
    (with-temp-buffer
      (org-mode)
      (insert "#+title: Tasks\n"
              "#+filetags: :work:\n"
              "#+filetags: :personal:\n"
              "* TODO Pending\n")
      (eds-org-agenda-enable-sync)
      (run-hooks 'before-save-hook)
      (expect (buffer-string)
              :to-equal (concat "#+title: Tasks\n"
                                "#+filetags: :work:agenda:\n"
                                "#+filetags: :personal:\n"
                                "* TODO Pending\n"))))

  (it "adds a missing agenda marker after the title"
    (with-temp-buffer
      (org-mode)
      (insert "#+title: Tasks\n* TODO Pending\n")
      (eds-org-agenda-enable-sync)
      (run-hooks 'before-save-hook)
      (expect (buffer-string)
              :to-equal (concat "#+title: Tasks\n"
                                "#+filetags: :agenda:\n"
                                "* TODO Pending\n"))))

  (it "removes a stale marker without changing unrelated FILETAGS"
    (with-temp-buffer
      (org-mode)
      (insert "#+filetags: :work:agenda:\n"
              "#+FILETAGS: :personal:\n"
              "* DONE Finished\n")
      (eds-org-agenda-enable-sync)
      (run-hooks 'before-save-hook)
      (expect (buffer-string)
              :to-equal (concat "#+filetags: :work:\n"
                                "#+FILETAGS: :personal:\n"
                                "* DONE Finished\n"))))

  (it "marks an explicit agenda file even without active work"
    (let* ((directory (make-temp-file "eds-org-agenda-" t))
           (file (expand-file-name "calendar.org" directory)))
      (unwind-protect
          (with-temp-buffer
            (org-mode)
            (setq buffer-file-name file)
            (insert "#+title: Calendar\n")
            (spy-on 'eds-org/get-org-directory :and-return-value directory)
            (eds-org-agenda-enable-sync)
            (run-hooks 'before-save-hook)
            (expect (buffer-string)
                    :to-equal "#+title: Calendar\n#+filetags: :agenda:\n"))
        (delete-directory directory t))))

  (it "preserves nested and source-block FILETAGS and does not refresh"
    (with-temp-buffer
      (org-mode)
      (insert "#+filetags: :agenda:\n"
              "#+begin_src org\n#+filetags: :source:\n#+end_src\n"
              "* Notes\n#+filetags: :nested:\n")
      (spy-on 'eds-org-agenda-refresh)
      (eds-org-agenda-enable-sync)
      (run-hooks 'before-save-hook)
      (expect (buffer-string)
              :to-equal (concat "#+filetags: :\n"
                                "#+begin_src org\n#+filetags: :source:\n#+end_src\n"
                                "* Notes\n#+filetags: :nested:\n"))
      (expect 'eds-org-agenda-refresh :not :to-have-been-called))))

(describe "eds-org-agenda-refresh"
  (it "sets deduplicated agenda files from Vulpea markers and explicit files"
    (let* ((directory (make-temp-file "eds-org-agenda-" t))
           (calendar (expand-file-name "calendar.org" directory))
           (work (expand-file-name "work.org" directory))
           (org-agenda-files nil))
      (unwind-protect
          (progn
            (spy-on 'eds-org/get-org-directory :and-return-value directory)
            (spy-on 'vulpea-db-query-by-tags-some
                    :and-return-value (list 'calendar-note 'work-note))
            (spy-on 'vulpea-note-path
                    :and-call-fake (lambda (note)
                                     (if (eq note 'calendar-note)
                                         calendar
                                       work)))
            (expect (eds-org-agenda-refresh)
                    :to-equal (list calendar work))
            (expect org-agenda-files :to-equal (list calendar work)))
        (delete-directory directory t)))))

(describe "eds-org-agenda-repair"
  (it "reports missing and stale markers without writing in dry-run mode"
    (let* ((directory (make-temp-file "eds-org-agenda-" t))
           (missing (expand-file-name "missing.org" directory))
           (stale (expand-file-name "stale.org" directory)))
      (unwind-protect
          (progn
            (write-region "* TODO Pending\n" nil missing nil 'silent)
            (write-region "#+filetags: :agenda:work:\n* DONE Finished\n"
                          nil stale nil 'silent)
            (spy-on 'eds-org/get-org-directory :and-return-value directory)
            (let ((result (eds-org-agenda-repair)))
              (expect (plist-get result :would-change)
                      :to-equal (list missing stale))
              (expect (plist-get result :changed) :to-equal nil)
              (expect (with-temp-buffer
                        (insert-file-contents missing)
                        (buffer-string))
                      :to-equal "* TODO Pending\n")
              (expect (with-temp-buffer
                        (insert-file-contents stale)
                        (buffer-string))
                      :to-equal
                      "#+filetags: :agenda:work:\n* DONE Finished\n")))
        (delete-directory directory t))))

  (it "writes repairs only when apply is non-nil"
    (let* ((directory (make-temp-file "eds-org-agenda-" t))
           (file (expand-file-name "tasks.org" directory)))
      (unwind-protect
          (progn
            (write-region "* TODO Pending\n" nil file nil 'silent)
            (spy-on 'eds-org/get-org-directory :and-return-value directory)
            (let ((result (eds-org-agenda-repair t)))
              (expect (plist-get result :changed) :to-equal (list file))
              (expect (plist-get result :would-change) :to-equal nil)
              (expect (with-temp-buffer
                        (insert-file-contents file)
                        (buffer-string))
                      :to-equal "#+filetags: :agenda:\n* TODO Pending\n")))
        (delete-directory directory t))))

  (it "skips modified visiting buffers"
    (let* ((directory (make-temp-file "eds-org-agenda-" t))
           (file (expand-file-name "tasks.org" directory))
           buffer)
      (unwind-protect
          (progn
            (write-region "* TODO Pending\n" nil file nil 'silent)
            (setq buffer (find-file-noselect file))
            (with-current-buffer buffer
              (goto-char (point-max))
              (insert "Unsaved\n"))
            (spy-on 'eds-org/get-org-directory :and-return-value directory)
            (let ((result (eds-org-agenda-repair t)))
              (expect (plist-get result :skipped) :to-equal (list file))
              (expect (plist-get result :changed) :to-equal nil)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (set-buffer-modified-p nil))
          (kill-buffer buffer))
        (delete-directory directory t))))

  (it "records a file failure and continues repairing later files"
    (let* ((directory (make-temp-file "eds-org-agenda-" t))
           (broken (expand-file-name "a-broken.org" directory))
           (good (expand-file-name "z-good.org" directory))
           (original-insert-file-contents
            (symbol-function 'insert-file-contents)))
      (unwind-protect
          (progn
            (write-region "* TODO Broken\n" nil broken nil 'silent)
            (write-region "* TODO Good\n" nil good nil 'silent)
            (spy-on 'eds-org/get-org-directory :and-return-value directory)
            (spy-on 'insert-file-contents
                    :and-call-fake
                    (lambda (filename &rest arguments)
                      (if (string-equal filename broken)
                          (error "Unreadable")
                        (apply original-insert-file-contents
                               filename arguments))))
            (let ((result (eds-org-agenda-repair)))
              (expect (plist-get result :failed)
                      :to-equal (list (cons broken "Unreadable")))
              (expect (plist-get result :would-change)
                      :to-equal (list good))))
        (delete-directory directory t)))))

(provide 'test-eds-org-agenda)

;;; test-eds-org-agenda.el ends here
