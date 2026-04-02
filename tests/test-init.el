;;; test-init.el --- Buttercup tests for init  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; 1. Set up undercover FIRST, before any source files are loaded
(load-file "tests/undercover-init.el")

;; 2. Load the source files to be instrumented
(require 'eds-blog)
(require 'eds-email)
(require 'eds-org)
(require 'eds-utils)

;; 3. Now load buttercup and the test files
(require 'buttercup)
(require 'test-eds-blog)
(require 'test-eds-email)
(require 'test-eds-org)
(require 'test-eds-utils)

;;; test-init.el ends here
