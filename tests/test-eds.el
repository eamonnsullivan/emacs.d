;; -*- lexical-binding: t; -*-

(require 'eds "../lisp/eds.el")

(describe "eds/strip-invalid-chars"
  (it "removes question marks"
    (expect (eds/strip-invalid-chars "Hello, World?") :to-equal "Hello, World"))
  (it "removes angle brackets"
    (expect (eds/strip-invalid-chars "This is a <test> string") :to-equal "This is a test string"))
  (it "leaves valid strings unchanged"
    (expect (eds/strip-invalid-chars "NoInvalidChars") :to-equal "NoInvalidChars"))
  (it "removes special characters except @, #, and %"
    (expect (eds/strip-invalid-chars "Special@:#%&*Chars") :to-equal "Special@#%Chars"))
  (it "removes slashes"
    (expect (eds/strip-invalid-chars "some/directory/like/thing") :to-equal "somedirectorylikething")))

(describe "eds/process-title"
  (it "processes title strings correctly"
    (expect (eds/process-title "Hello, World?") :to-equal "hello,-world")
    (expect (eds/process-title "This is a <test> string!") :to-equal "this-is-a-test-string")
    (expect (eds/process-title "NoInvalidChars") :to-equal "noinvalidchars")
    (expect (eds/process-title "Special@:#%&*Chars") :to-equal "special@#%chars")
    (expect (eds/process-title "some/directory/like/thing") :to-equal "somedirectorylikething")))

(describe "eds/get-from-field"
  (it "get the from field from a message plist"
    (expect (eds/get-from-field '(:from ((:name "User" :email "user@example.com"))))
            :to-equal '((:name "User" :email "user@example.com"))))
    (it "returns nil when from field is absent"
      (expect (eds/get-from-field '(:subject "Test Subject" :date "2024-06-01"))
              :to-equal nil))
    (it "returns the from field when it's a simple string"
      (expect (eds/get-from-field '(:from "something"))
              :to-equal "something")))

(describe "eds/get-contact-email"
  (it "returns the email address part of a contact"
    (expect (eds/get-contact-email '(:email "user@example.com" :name "User"))
            :to-equal "user@example.com"))
  (it "returns nil when no email is present"
    (expect (eds/get-contact-email '(:name "User"))
            :to-be nil)))

(describe "eds/get-mu4e-from-string-search"
  (it "returns a search string with the sender's email"
    (let ((msg '(:docid 32461
                        :from ((:name "Nikola Tesla" :email "niko@example.com"))
                        :to ((:name "Thomas Edison" :email "tom@example.com"))
                        :cc ((:name "Rupert The Monkey" :email "rupert@example.com"))
                        :subject "RE: what about the 50K?"
                        :date (20369 17624 0)
                        :size 4337
                        :message-id "238C8233AB82D81EE81AF0114E4E74@123213.mail.example.com"
                        :path  "/home/tom/Maildir/INBOX/cur/133443243973_1.10027.atlas:2,S"
                        :maildir "/INBOX"
                        :priority normal
                        :flags (seen))))
      (expect (eds/get-mu4e-from-search-string msg)
              :to-equal "from:niko@example.com"))))

(describe "eds/extract-jira-ticket"
  (it "extracts things that look like a ticket"
    (expect (eds/extract-jira-ticket "feature/ABC-123-some-feature") :to-equal "ABC-123")
    (expect (eds/extract-jira-ticket "PROJECT-123-some-feature") :to-equal "PROJECT-123")
    (expect (eds/extract-jira-ticket "ABC-4567-fix-bug") :to-equal "ABC-4567")
    (expect (eds/extract-jira-ticket "XYZ-89-another-task") :to-equal "XYZ-89"))
  (it "return INNOVATION-DAY with things that look like that"
    (expect (eds/extract-jira-ticket "Innovation-Day") :to-equal "INNOVATION-DAY")
    (expect (eds/extract-jira-ticket "innovation-day-something") :to-equal "INNOVATION-DAY"))
  (it "returns COP-DAY for cop-day strings"
    (expect (eds/extract-jira-ticket "cop-day-something") :to-equal "COP-DAY"))
  (it "returns NO-TICKET when no ticket is found"
    (expect (eds/extract-jira-ticket "not-a-jira-ticket") :to-equal "NO-TICKET")))

(describe "eds/get-org-directory"
  :var (file-truename)

  (before-each
  (spy-on 'file-truename
          :and-return-value
          "/Users/sullie09/Library/CloudStorage/Dropbox/org"))

  (it "returns the full path to the org directory"
    (expect (eds/get-org-directory)
            :to-equal "/Users/sullie09/Library/CloudStorage/Dropbox/org")))

(describe "eds/filter-for-regex"
  (it "filters a list of strings based on a regex"
    (expect (eds/filter-for-regex "foo" '("foobar" "baz" "fool23" "hello"))
            :to-equal '("foobar" "fool23"))
    (expect (eds/filter-for-regex "foob" '("foobar" "baz" "fool23" "hello"))
            :to-equal '("foobar"))
    (expect (eds/filter-for-regex "foo.[0-9]" '("foobar" "baz" "fool23" "hello"))
            :to-equal '("fool23"))
    (expect (eds/filter-for-regex "foo" '("baz" "hello"))
            :to-be nil)))

(describe "eds/get-conflicted-org-files"
  :var (directory-files eds/get-org-directory)

  (before-each
    (fset 'eds/get-org-directory (lambda ()
                                   ""))
    (fset 'directory-files (lambda (dir)
                                   '()))
    (spy-on 'eds/get-org-directory
            :and-return-value "/mock/org/dir")
    (spy-on 'directory-files
            :and-return-value '("notes.org"
                                "meeting-conflicted.org"
                                "tasks.org"
                                "project-conflicted-v2.org")))

  (it "returns only files with 'conflicted' in their names"
    (expect (eds/get-conflicted-org-files)
            :to-equal '("meeting-conflicted.org" "project-conflicted-v2.org"))))

(describe "eds/link-to-svp-contact-page"
  (it "inserts a contact link around the provided text"
    (expect (eds/link-to-svp-contact-page "test")
            :to-equal "[test](../../pages-output/contact/)")
    (expect (eds/link-to-svp-contact-page "Hello World")
            :to-equal "[Hello World](../../pages-output/contact/)"))
  (it "returns nil for empty or nil input"
    (expect (eds/link-to-svp-contact-page "")
            :to-be nil)
    (expect (eds/link-to-svp-contact-page nil)
            :to-be nil)))

(describe "eds/get-sendmail-extra-args"
  (it "handles the eamonn.sullivan gmail account"
    (expect (eds/get-sendmail-extra-args "eamonn.sullivan@gmail.com")
            :to-equal '("-a" "gmail-eamonn")))
  (it "handles the svp account"
    (expect (eds/get-sendmail-extra-args "svpsouthruislip@gmail.com")
            :to-equal '("-a" "gmail-svp")))
  (it "handles the fastmail account"
    (expect (eds/get-sendmail-extra-args "me@eamonnsullivan.co.uk")
            :to-equal '("-a" "fastmail")))
  (it "handles other emails send to fastmail"
    (expect (eds/get-sendmail-extra-args "something@eamonnsullivan.co.uk")
            :to-equal '("-a" "fastmail")))
  (it "returns nil for unknown email addresses"
    (expect (eds/get-sendmail-extra-args "user@example.com")
            :to-be nil)))
