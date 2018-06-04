;;; -*- lexical-binding: t -*-
;;; eds-test.el --- unit tests for my tweaks for various packages

(ert-deftest eds/test-test-or-impl ()
  "Test the output of eds/get-test-or-impl."
  (should (equal (eds/get-test-or-impl "/dir/somewhere/something.test.js") "/dir/somewhere/something.js"))
  (should (equal (eds/get-test-or-impl "/dir.test/somewhere.test.js/something.test.js") "/dir.test/somewhere.test.js/something.js"))
  (should (equal (eds/get-test-or-impl "unexpected.doc") "unexpected.doc")))

(ert-deftest eds/test-is-js ()
  "Test that we correctly recognize javascript buffers."
  (should (equal (eds/is-js "/dir/somewhere/something.js") 't))
  (should (equal (eds/is-js "something.else") nil)))

(provide 'eds-test)
