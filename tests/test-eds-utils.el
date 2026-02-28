;;; test-eds-utils.el --- unit tests for eds-utils.el  -*- lexical-binding:t -*-
;;
;; Copyright (C) 2026 Eamonnn Sullivan
;;
;;
;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Version: 1.0
;; Keywords: emacs
;; URL: https://eamonnsullivan.co.uk
;;
;;
;;; Commentary:
;;
;;
;;; Code:

(require 'eds-utils)

(describe "eds-utils/kill-word"
  (it "deletes the next word when no region is selected"
    (with-temp-buffer
      (insert "Hello World!")
      (goto-char (point-min))
      (eds-utils/kill-word 1)
      (expect (buffer-string) :to-equal " World!")))

  (it "deletes the previous word when no region is selected"
    (with-temp-buffer
      (insert "Hello World!")
      (goto-char (point-max))
      (eds-utils/backward-kill-word 1)
      (expect (buffer-string) :to-equal "Hello ")))

  (it "deletes the selected region"
    (with-temp-buffer
      (insert "Hello World!")
      (goto-char (point-min))
      (set-mark-command nil)
      (forward-word 1)
      (eds-utils/kill-word 1)
      (expect (buffer-string) :to-equal "Hello!")))

  (it "deletes the specified number of words"
    (with-temp-buffer
      (insert "The quick brown fox jumps over the lazy dog.")
      (goto-char (point-min))
      (eds-utils/kill-word 3)
      (expect (buffer-string) :to-equal " fox jumps over the lazy dog."))))

(describe "eds-utils/backward-kill-word"
  (it "deletes the previous word when no region is selected"
    (with-temp-buffer
      (insert "Hello World!")
      (goto-char (point-max))
      (eds-utils/backward-kill-word 1)
      (expect (buffer-string) :to-equal "Hello ")))

  (it "deletes the next word when no region is selected"
    (with-temp-buffer
      (insert "Hello World!")
      (goto-char (point-min))
      (eds-utils/backward-kill-word -1)
      (expect (buffer-string) :to-equal " World!")))

  (it "deletes the selected region"
    (with-temp-buffer
      (insert "Hello World!")
      (goto-char (point-min))
      (set-mark-command nil)
      (forward-word 1)
      (eds-utils/backward-kill-word 1)
      (expect (buffer-string) :to-equal " World!")))

  (it "deletes the specified number of words"
    (with-temp-buffer
      (insert "The quick brown fox jumps over the lazy dog.")
      (goto-char (point-max))
      (eds-utils/backward-kill-word 3)
      (expect (buffer-string) :to-equal "The quick brown fox jumps over "))))

(describe "eds-utils/switch-to-previous-buffer"
  (it "switches to the previously open buffer"
    (with-temp-buffer
      (let ((buf1 (generate-new-buffer "*temp-buffer-1*"))
            (buf2 (generate-new-buffer "*temp-buffer-2*")))
        (switch-to-buffer buf1)
        (switch-to-buffer buf2)
        (eds-utils/switch-to-previous-buffer)
        (expect (current-buffer) :to-equal buf1)
        (eds-utils/switch-to-previous-buffer)
        (expect (current-buffer) :to-equal buf2)
        (kill-buffer buf2)))))

(describe "eds-utils/strip-invalid-chars"
  (it "removes question marks"
    (expect (eds-utils/strip-invalid-chars "Hello, World?") :to-equal "Hello, World"))
  (it "removes angle brackets"
    (expect (eds-utils/strip-invalid-chars "This is a <test> string") :to-equal "This is a test string"))
  (it "leaves valid strings unchanged"
    (expect (eds-utils/strip-invalid-chars "NoInvalidChars") :to-equal "NoInvalidChars"))
  (it "removes special characters except @, #, and %"
    (expect (eds-utils/strip-invalid-chars "Special@:#%&*Chars") :to-equal "Special@#%Chars"))
  (it "removes slashes"
    (expect (eds-utils/strip-invalid-chars "some/directory/like/thing") :to-equal "somedirectorylikething")))

(describe "eds-utils/process-title"
  (it "removes question marks and replaces spaces with hyphens"
    (expect (eds-utils/process-title "Hello, World?") :to-equal "hello,-world"))
  (it "removes angle brackets"
    (expect (eds-utils/process-title "This is a <test> string!") :to-equal "this-is-a-test-string"))
  (it "it downcases"
    (expect (eds-utils/process-title "NoInvalidChars") :to-equal "noinvalidchars"))
  (it "removes some special characters"
    (expect (eds-utils/process-title "Special@:#%&*Chars") :to-equal "special@#%chars"))
  (it "removes forward slashes"
    (expect (eds-utils/process-title "some/directory/like/thing") :to-equal "somedirectorylikething")))

(describe "eds-utils/filter-for-regex"
  (it "filters a list of strings based on a regex"
    (expect (eds-utils/filter-for-regex "foo" '("foobar" "baz" "fool23" "hello"))
            :to-equal '("foobar" "fool23"))
    (expect (eds-utils/filter-for-regex "foob" '("foobar" "baz" "fool23" "hello"))
            :to-equal '("foobar"))
    (expect (eds-utils/filter-for-regex "foo.[0-9]" '("foobar" "baz" "fool23" "hello"))
            :to-equal '("fool23"))
    (expect (eds-utils/filter-for-regex "foo" '("baz" "hello"))
            :to-be nil)))

(describe "eds-utils/eds-insert-git-branch-name"
  :var (magit-get-current-branch)
  (before-each
    (setq eds-insert-branch-name-p t))
  (after-each
    (setq eds-insert-branch-name-p nil))

  (it "it extracts the jira ticket from the branch"
    (spy-on 'magit-get-current-branch
            :and-return-value "feature/ABC-123-new-feature")
    (with-temp-buffer
      (let ((initial-point (point)))
        (eds-utils/insert-git-branch-name)
        (expect (buffer-string) :to-equal "[ABC-123] ")
        (expect (point) :to-equal (+ initial-point 10))
        (expect eds-insert-branch-name-p :to-be nil)))))

(describe "eds-utils/extract-jira-ticket"
  (it "extracts things that look like a ticket"
    (expect (eds-utils/extract-jira-ticket "feature/ABC-123-some-feature") :to-equal "ABC-123")
    (expect (eds-utils/extract-jira-ticket "PROJECT-123-some-feature") :to-equal "PROJECT-123")
    (expect (eds-utils/extract-jira-ticket "ABC-4567-fix-bug") :to-equal "ABC-4567")
    (expect (eds-utils/extract-jira-ticket "XYZ-89-another-task") :to-equal "XYZ-89"))
  (it "ignores case when spotting a ticket"
    (expect (eds-utils/extract-jira-ticket "passports-123") :to-equal "PASSPORTS-123")
    (expect (eds-utils/extract-jira-ticket "PassPorts-123") :to-equal "PASSPORTS-123"))
  (it "return INNOVATION-DAY with things that start with 'innovation'"
    (expect (eds-utils/extract-jira-ticket "Innovation-Day") :to-equal "INNOVATION-DAY")
    (expect (eds-utils/extract-jira-ticket "innovation-day-something") :to-equal "INNOVATION-DAY"))
  (it "returns COP-DAY for strings that start with 'cop-day"
    (expect (eds-utils/extract-jira-ticket "cop-day-something") :to-equal "COP-DAY"))
  (it "returns NO-TICKET when no ticket is found"
    (expect (eds-utils/extract-jira-ticket "not-a-jira-ticket") :to-equal "NO-TICKET")))

(describe "eds-utils/filter-buffer-list"
  (before-each
    (if (get-buffer "*temp-buffer1*") (kill-buffer "*temp-buffer1*"))
    (if (get-buffer "*temp-buffer2*") (kill-buffer "*temp-buffer2*")))
  (it "filters out promidor buffer"
    (let ((buffers (list (generate-new-buffer "*temp-buffer1*")
                         (generate-new-buffer "*temp-buffer2*")
                         (generate-new-buffer "*pomidor*"))))
      (expect (mapcar #'buffer-name (eds-utils/filter-buffer-list buffers))
              :to-equal '("*temp-buffer1*" "*temp-buffer2*")))))

(provide 'test-eds-utils)
;;; test-eds-utils.el ends here
