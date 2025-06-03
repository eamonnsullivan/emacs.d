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

(provide 'eds-test)
