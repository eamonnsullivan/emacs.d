;;; test-eds-package-usage.el --- Tests for package usage -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for aggregate package command tracking and persistence.

;;; Code:

(load-file "tests/setup.el")

(require 'eds-package-usage)

(describe "eds-package-usage"
  (before-each
    (setq eds-package-usage--records (make-hash-table :test #'eq)
          eds-package-usage--origin-cache (make-hash-table :test #'eq)
          eds-package-usage--dirty nil))

  (it "attributes files in straight build directories"
    (let ((user-emacs-directory "/tmp/example-emacs/"))
      (expect
       (eds-package-usage--package-from-file
        "/tmp/example-emacs/straight/build/magit/magit.el")
       :to-equal "magit")))

  (it "attributes files in named straight build directories"
    (let ((user-emacs-directory "/tmp/example-emacs/"))
      (expect
       (eds-package-usage--package-from-file
        "/tmp/example-emacs/straight/build-work/consult/consult.el")
       :to-equal "consult")))

  (it "ignores files outside straight"
    (let ((user-emacs-directory "/tmp/example-emacs/"))
      (expect
       (eds-package-usage--package-from-file
        "/Applications/Emacs.app/Contents/Resources/lisp/simple.el")
       :to-be nil)))

  (it "aggregates counts and distinct days"
    (eds-package-usage--record 'magit-status "magit"
                               (encode-time 0 0 12 1 1 2026))
    (eds-package-usage--record 'magit-status "magit"
                               (encode-time 0 0 13 1 1 2026))
    (eds-package-usage--record 'magit-status "magit"
                               (encode-time 0 0 12 2 1 2026))
    (let ((record (gethash 'magit-status eds-package-usage--records)))
      (expect (plist-get record :count) :to-equal 3)
      (expect (length (plist-get record :days)) :to-equal 2)))

  (it "honours explicit attribution overrides"
    (let ((eds-package-usage-attribution-overrides
           '((generated-suffix . "magit"))))
      (expect (eds-package-usage--command-package 'generated-suffix)
              :to-equal "magit")))

  (it "round-trips state through the configured file"
    (let* ((directory (make-temp-file "eds-package-usage-" t))
           (eds-package-usage-state-file
            (expand-file-name "usage.el" directory)))
      (unwind-protect
          (progn
            (eds-package-usage--record 'consult-line "consult"
                                       (encode-time 0 0 12 1 1 2026))
            (eds-package-usage-save)
            (setq eds-package-usage--records
                  (make-hash-table :test #'eq))
            (eds-package-usage-load)
            (expect (plist-get
                     (gethash 'consult-line eds-package-usage--records)
                     :count)
                    :to-equal 1))
        (delete-directory directory t))))

  (it "marks an installed package without observations for review"
    (spy-on 'eds-package-usage--installed-packages
            :and-return-value '("unused-package"))
    (let* ((entry (car (eds-package-usage--report-entries)))
           (columns (cadr entry)))
      (expect (aref columns 0) :to-equal "unused-package")
      (expect (aref columns 1) :to-equal "0")
      (expect (aref columns 2) :to-equal "0")
      (expect (aref columns 3) :to-equal "never")
      (expect (aref columns 5) :to-equal "review")))

  (it "renders numeric usage values as tabulated-list cells"
    (spy-on 'eds-package-usage--installed-packages
            :and-return-value '("unused-package"))
    (eds-package-usage--record 'consult-line "consult"
                               (encode-time 0 0 12 1 1 2026))
    (with-temp-buffer
      (eds-package-usage-report-mode)
      (eds-package-usage--refresh-report)
      (expect (tabulated-list-print t) :not :to-throw)
      (expect (buffer-string) :to-match "consult")
      (expect (buffer-string) :to-match "unused-package")))

  (it "sorts usage columns numerically"
    (let ((two '("two" ["two" "2" "1" "never" "" "observed"]))
          (ten '("ten" ["ten" "10" "3" "never" "" "observed"])))
      (expect (funcall (eds-package-usage--make-column-sorter 1) two ten)
              :to-be-truthy)
      (expect (funcall (eds-package-usage--make-column-sorter 2) two ten)
              :to-be-truthy))))

(provide 'test-eds-package-usage)

;;; test-eds-package-usage.el ends here
