;;; test-init.el --- Buttercup tests for init  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; (setq undercover-force-coverage t)
;; (when (require 'undercover nil t)
;;   (undercover "lisp/eds-*.el"
;;               (:report-file "coverage/.resultset.json")
;;               (:report-format 'simplecov)
;;               (:send-report nil)))

(load-file "./tests/undercover-init.el")

(require 'buttercup)

;;; test-init.el ends here
