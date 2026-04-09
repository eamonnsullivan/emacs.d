;;; setup.el --- Set up unit testing, including code coverage -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonnn Sullivan

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Version: 0.1
;; Keywords: emacs buttercup testing coverage
;; URL: https://eamonnsullivan.co.uk

;;; Commentary:

;; Test bootstrap for Buttercup + Undercover.

;;; Code:

(defvar eds-testing-setup nil
  "Non-nil when the test environment has already been initialized.")

(defun eds-initialize-test-environment ()
  "Initialize the test environment, including code coverage."
  (unless eds-testing-setup
    (setq eds-testing-setup t)

    ;; Must be set before loading Undercover/instrumented files.
    (setq undercover-force-coverage t
          undercover--verbosity 7)

    (when (require 'undercover nil t)
      (undercover "lisp/eds-*.el"
                  (:report-file "coverage/.resultset.json")
                  (:report-format 'simplecov)
                  (:send-report nil)))

    ;; Load instrumented libraries after Undercover is initialized.
    (let ((load-prefer-newer t))
      (require 'eds-blog)
      (require 'eds-email)
      (require 'eds-org)
      (require 'eds-utils))))

(eds-initialize-test-environment)

(provide 'setup)

;;; setup.el ends here
