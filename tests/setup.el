;;; setup.el --- Set up unit testing, including code coverage -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonnn Sullivan

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Version: 0.1
;; Keywords: emacs buttercup testing coverage
;; URL: https://eamonnsullivan.co.uk

;;; Commentary:

;; Test bootstrap for Buttercup + Undercover.  Coverage is collected when
;; Undercover detects CI or the UNDERCOVER_FORCE environment variable is set.

;;; Code:

(defvar eds-testing-setup nil
  "Non-nil when the test environment has already been initialized.")

(defun eds-initialize-test-environment ()
  "Initialize the test environment, including code coverage."
  (unless eds-testing-setup
    (setq eds-testing-setup t)

    ;; Undercover must be configured before loading instrumented files.
    (require 'undercover)
    (undercover "lisp/eds-*.el"
                (:report-file "coverage/.resultset.json")
                (:report-format 'simplecov)
                (:send-report nil)
                (:merge-report nil))

    ;; Load instrumented libraries after Undercover is initialized.
    (let ((load-prefer-newer t))
      (require 'eds-blog)
      (require 'eds-email)
      (require 'eds-org)
      (require 'eds-utils)
      (require 'eds-github))))

(eds-initialize-test-environment)

(provide 'setup)

;;; setup.el ends here
