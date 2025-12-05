;;; -*- lexical-binding: t -*-
;;; eds-test.el --- unit tests for my tweaks for various packages

(ert-deftest eds/test-get-test-or-impl ()
  "Test the output of eds/get-test-or-impl."
  (should (equal (eds/get-test-or-impl "/dir/somewhere/something.test.js") "/dir/somewhere/something.js"))
  (should (equal (eds/get-test-or-impl "/dir.test/somewhere.test.js/something.test.js") "/dir.test/somewhere.test.js/something.js"))
  (should (equal (eds/get-test-or-impl "unexpected.doc") "unexpected.doc")))

(ert-deftest eds/test-is-js ()
  "Test that we correctly recognize javascript buffers."
  (should (equal (eds/is-js "/dir/somewhere/something.js") 't))
  (should (equal (eds/is-js "something.else") nil)))

(ert-deftest eds/test-extract-jira-ticket ()
  "Test that we correctly find something that looks like a JIRA ticket in a string"
  (should (equal (eds/extract-jira-ticket "cpsroadmap-1234") "CPSROADMAP-1234"))
  (should (equal (eds/extract-jira-ticket "not-a-jira-ticket") "NO-TICKET"))
  (should (equal (eds/extract-jira-ticket "a") "NO-TICKET"))
  (should (equal (eds/extract-jira-ticket "") "NO-TICKET"))
  (should (equal (eds/extract-jira-ticket "CPSROADMAP-1234-something-other-stuff") "CPSROADMAP-1234"))
  (should (equal (eds/extract-jira-ticket "someStuff-cpsRoadMap-1234-someOther/stuff") "CPSROADMAP-1234"))
  (should (equal (eds/extract-jira-ticket "innovation-day-something") "INNOVATION-DAY"))
  (should (equal (eds/extract-jira-ticket "Innovation-Day") "INNOVATION-DAY"))
  (should (equal (eds/extract-jira-ticket "cop-day-something") "COP-DAY")))

(ert-deftest eds/test-get-org-directory ()
  "Test that we correctly return the org directory on Linux and the Mac"
    (if (eq system-type 'darwin)
        (should (equal (eds/get-org-directory) "/Users/sullie09/Library/CloudStorage/Dropbox/org"))
      (should (equal (eds/get-org-directory) "/home/eamonn/Dropbox/org"))))

(ert-deftest eds/filter-for-regex ()
  "Tests that we correctly filter a list of string"
  (should (equal (eds/filter-for-regex "foo" '("foobar" "baz" "fool23" "hello"))
                 '("foobar" "fool23")))
  (should (equal (eds/filter-for-regex "foob" '("foobar" "baz" "fool23" "hello"))
                 '("foobar")))
  (should (equal (eds/filter-for-regex "foo.[0-9]" '("foobar" "baz" "fool23" "hello"))
                 '("fool23")))
  (should (equal (eds/filter-for-regex "foo" '("baz" "hello"))
                 nil)))

(ert-deftest eds/test-make-svp-contact-link ()
  "Tests the function for inserting the contact link in the current region"
  (with-temp-buffer
    (insert "This is a test")
    (set-mark 11)
    (goto-char (point-max))
    (call-interactively 'eds/make-svp-contact-link)
    (should (equal "This is a [test](../../pages-output/contact/)"
                   (buffer-string))))
  ;; Makes no changes when there is no region
  (with-temp-buffer
    (insert "This is a test")
    (call-interactively 'eds/make-svp-contact-link)
    (should (equal "This is a test"
                   (buffer-string))))
  ;; Can handle the whole buffer region
  (with-temp-buffer
    (insert "This is a test")
    (set-mark (point-min))
    (goto-char (point-max))
    (call-interactively 'eds/make-svp-contact-link)
    (should (equal "[This is a test](../../pages-output/contact/)"
                   (buffer-string)))))

(ert-deftest eds/get-from-field ()
    "Test getting the from field from a message plist"
  (should (equal (eds/get-from-field '(:from ((:name "User" :email "user@example.com"))))
                 '((:name "User" :email "user@example.com"))))
  (should (equal (eds/get-from-field '(:subject "Test Subject" :date "2024-06-01"))
                 nil))
  (should (equal (eds/get-from-field '(:from "something"))
                 "something")))

(ert-deftest eds/get-contact-email ()
  "Test that we extract just the email from the first contact"
  (should (equal (eds/get-contact-email '(:email "user@example.com" :name "User"))
                 "user@example.com"))
  (should (equal (eds/get-contact-email '(:name "User"))
                 nil)))

(ert-deftest eds/get-email-search-string ()
  (should (equal (eds/get-email-search-string "user@example.com")
                 "from:user@example.com")))

(provide 'eds-test)
