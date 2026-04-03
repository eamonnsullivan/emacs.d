;;; setup.el --- set up unit testing, including code coverage  -*- lexical-binding:t -*-
;;
;; Copyright (C) 2026 Eamonnn Sullivan
;;
;;
;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Version: 0.1
;; Keywords: emacs buttercup testing coverage
;; URL: https://eamonnsullivan.co.uk
;;
;;
;;; Commentary:
;;
;;
;;; Code:

(defvar eds-testing-setup nil
  "Flag to indicate whether the testing setup has been initialized.")

(setq undercover-force-coverage t
      undercover--verbosity 7)
(when (and (require 'undercover nil t) (not eds-testing-setup))
  (setq eds-testing-setup t)
  (undercover "lisp/eds-*.el"
              (:report-file "coverage/.resultset.json")
              (:report-format 'simplecov)
              (:send-report nil)))

(require 'eds-blog)
(require 'eds-email)
(require 'eds-org)
(require 'eds-utils)

(provide 'test-setup)

;;; setup.el ends here
