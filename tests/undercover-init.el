;; -*- lexical-binding: t; -*-

(when (require 'undercover nil t)
  (undercover "./lisp/eds-*.el"
              (:report-file "./coverage/.resultset.json")
              (:report-format 'simplecov)
              (:send-report nil)))
