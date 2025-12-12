;; -*- lexical-binding: t; -*-

(require 'eds)

(describe "eds/eds-insert-git-branch-name"
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
        (eds/insert-git-branch-name)
        (expect (buffer-string) :to-equal "[ABC-123] ")
        (expect (point) :to-equal (+ initial-point 10))
        (expect eds-insert-branch-name-p :to-be nil)))))

(describe "eds/kill-word"
  (it "deletes the next word when no region is selected"
    (with-temp-buffer
      (insert "Hello World!")
      (goto-char (point-min))
      (eds/kill-word 1)
      (expect (buffer-string) :to-equal " World!")))

  (it "deletes the previous word when no region is selected"
    (with-temp-buffer
      (insert "Hello World!")
      (goto-char (point-max))
      (eds/backward-kill-word 1)
      (expect (buffer-string) :to-equal "Hello ")))

  (it "deletes the selected region"
    (with-temp-buffer
      (insert "Hello World!")
      (goto-char (point-min))
      (set-mark-command nil)
      (forward-word 1)
      (eds/kill-word 1)
      (expect (buffer-string) :to-equal "Hello!")))

  (it "deletes the specified number of words"
    (with-temp-buffer
      (insert "The quick brown fox jumps over the lazy dog.")
      (goto-char (point-min))
      (eds/kill-word 3)
      (expect (buffer-string) :to-equal " fox jumps over the lazy dog."))))

(describe "eds/backward-kill-word"
  (it "deletes the previous word when no region is selected"
    (with-temp-buffer
      (insert "Hello World!")
      (goto-char (point-max))
      (eds/backward-kill-word 1)
      (expect (buffer-string) :to-equal "Hello ")))

  (it "deletes the next word when no region is selected"
    (with-temp-buffer
      (insert "Hello World!")
      (goto-char (point-min))
      (eds/backward-kill-word -1)
      (expect (buffer-string) :to-equal " World!")))

  (it "deletes the selected region"
    (with-temp-buffer
      (insert "Hello World!")
      (goto-char (point-min))
      (set-mark-command nil)
      (forward-word 1)
      (eds/backward-kill-word 1)
      (expect (buffer-string) :to-equal " World!")))

  (it "deletes the specified number of words"
    (with-temp-buffer
      (insert "The quick brown fox jumps over the lazy dog.")
      (goto-char (point-max))
      (eds/backward-kill-word 3)
      (expect (buffer-string) :to-equal "The quick brown fox jumps over "))))

(describe "eds/switch-to-previous-buffer"
  (it "switches to the previously open buffer"
    (with-temp-buffer
      (let ((buf1 (generate-new-buffer "*temp-buffer-1*"))
            (buf2 (generate-new-buffer "*temp-buffer-2*")))
        (switch-to-buffer buf1)
        (switch-to-buffer buf2)
        (eds/switch-to-previous-buffer)
        (expect (current-buffer) :to-equal buf1)
        (eds/switch-to-previous-buffer)
        (expect (current-buffer) :to-equal buf2)
        (kill-buffer buf2)))))

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
  (it "removes question marks and replaces spaces with hyphens"
    (expect (eds/process-title "Hello, World?") :to-equal "hello,-world"))
  (it "removes angle brackets"
    (expect (eds/process-title "This is a <test> string!") :to-equal "this-is-a-test-string"))
  (it "it downcases"
    (expect (eds/process-title "NoInvalidChars") :to-equal "noinvalidchars"))
  (it "removes some special characters"
    (expect (eds/process-title "Special@:#%&*Chars") :to-equal "special@#%chars"))
  (it "removes forward slashes"
    (expect (eds/process-title "some/directory/like/thing") :to-equal "somedirectorylikething")))

(describe "eds/get-from-field"
  (it "gets the 'from' field from a message"
    (expect (eds/get-from-field '(:from ((:name "User" :email "user@example.com"))))
            :to-equal '((:name "User" :email "user@example.com"))))
  (it "returns nil when 'from' field is absent"
    (expect (eds/get-from-field '(:subject "Test Subject" :date "2024-06-01"))
            :to-equal nil)))

(describe "eds/get-contact-email"
  (it "returns the email address part of a contact"
    (expect (eds/get-contact-email '(:email "user@example.com" :name "User"))
            :to-equal "user@example.com"))
  (it "returns nil when no email is present"
    (expect (eds/get-contact-email '(:name "User"))
            :to-be nil)))

(describe "eds/get-mu4e-from-string-search"
  (it "extracts sender's email from a msg and returns a mu4e seach string"
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
  (it "ignores case when spotting a ticket"
    (expect (eds/extract-jira-ticket "passports-123") :to-equal "PASSPORTS-123")
    (expect (eds/extract-jira-ticket "PassPorts-123") :to-equal "PASSPORTS-123"))
  (it "return INNOVATION-DAY with things that start with 'innovation'"
    (expect (eds/extract-jira-ticket "Innovation-Day") :to-equal "INNOVATION-DAY")
    (expect (eds/extract-jira-ticket "innovation-day-something") :to-equal "INNOVATION-DAY"))
  (it "returns COP-DAY for strings that start with 'cop-day"
    (expect (eds/extract-jira-ticket "cop-day-something") :to-equal "COP-DAY"))
  (it "returns NO-TICKET when no ticket is found"
    (expect (eds/extract-jira-ticket "not-a-jira-ticket") :to-equal "NO-TICKET")))

(describe "eds/start-blog-post"
  :var (find-file
        eds/insert-skeleton-blog-post
        save-buffer
        magit-branch-create
        magit-checkout)
  (before-all
    (fset 'find-file (lambda (filename) nil))
    (fset 'eds/insert-skeleton-blog-post (lambda (title) nil))
    (fset 'save-buffer (lambda () nil))
    (fset 'magit-branch-create (lambda (branch base) nil))
    (fset 'magit-checkout (lambda (branch) nil)))


  (it "creates a new blog post file with the correct name and branch"
    (let ((project "/mock/project")
          (title "My First Blog Post"))
      (spy-on 'find-file)
      (spy-on 'eds/insert-skeleton-blog-post)
      (spy-on 'save-buffer)
      (spy-on 'magit-branch-create)
      (spy-on 'magit-checkout)
      (eds/start-blog-post project title)
      (let* ((expected-branch (concat (format-time-string "%Y-%m-%d") "-my-first-blog-post"))
             (expected-filename (concat project "/content/md/posts/" expected-branch ".md")))
        (expect 'find-file :to-have-been-called-with expected-filename)
        (expect 'eds/insert-skeleton-blog-post :to-have-been-called-with title)
        (expect 'save-buffer :to-have-been-called)
        (expect 'magit-branch-create :to-have-been-called-with expected-branch "main")
        (expect 'magit-checkout :to-have-been-called-with expected-branch)))))

(describe "eds/get-org-directory"
  :var (file-truename)
  (before-each
    (spy-on 'file-truename
            :and-return-value
            "/mock/path/to/org"))

  (it "returns the full path to the org directory"
    (expect (eds/get-org-directory)
            :to-equal "/mock/path/to/org")))

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

(describe "eds/ref-link-org-roam"
  :var (find-file
        insert
        org-id-get-create
        org-set-property
        end-of-buffer
        save-buffer)
  (before-all
    (fset 'find-file (lambda (filename) nil))
    (fset 'insert (lambda (str) nil))
    (fset 'org-id-get-create (lambda () nil))
    (fset 'org-set-property (lambda (prop) nil))
    (fset 'end-of-buffer (lambda () nil))
    (fset 'save-buffer (lambda () nil)))

  (before-each
    (spy-on 'find-file)
    (spy-on 'insert)
    (spy-on 'org-id-get-create)
    (spy-on 'org-set-property)
    (spy-on 'end-of-buffer)
    (spy-on 'save-buffer)
    (eds/ref-link-org-roam "This is a test" "https://some.ref"))

  (it "opens a buffer"
    (expect 'find-file :to-have-been-called-times 1))

  (it "inserts the title and ref"
    (expect 'insert :to-have-been-called-with "#+title: This is a test\n#+startup: content\n* [[https://some.ref][This is a test]]\n"))

  (it "creates a new org-roam id"
    (expect 'org-id-get-create :to-have-been-called-times 1))

  (it "sets a ROAM_REFS property with the ref"
    (expect 'org-set-property :to-have-been-called-with "ROAM_REFS" "https://some.ref"))

  (it "saves the buffer"
    (expect 'save-buffer :to-have-been-called-times 1)))

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
