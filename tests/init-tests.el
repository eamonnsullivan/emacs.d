;; -*- lexical-binding: t; -*-

(setq undercover-force-coverage t)
(require 'undercover nil t)
(undercover "*.el" "lisp/*.el"
            (:report-file "coverage/.resultset.json")
            (:report-format 'simplecov)
            (:send-report nil))

(require 'eds-blog nil t)
(require 'eds-email nil t)
(require 'eds-org nil t)
(require 'eds-utils nil t)

(describe "tests are initialised correctly"
  (it "has loaded undercover"
    (expect (featurep 'undercover) :to-be t))
  (it "has undercover enabled"
    (expect (undercover-enabled-p) :to-be-truthy))
  (it "has loaded eds-blog"
    (expect (featurep 'eds-blog) :to-be t))
  (it "has loaded eds-email"
    (expect (featurep 'eds-email) :to-be t))
  (it "has loaded eds-org"
    (expect (featurep 'eds-org) :to-be t))
  (it "has loaded eds-utils"
    (expect (featurep 'eds-utils) :to-be t)))
