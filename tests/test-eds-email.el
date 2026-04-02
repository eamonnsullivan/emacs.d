;;; test-eds-email.el --- unit tests for eds-email.el  -*- lexical-binding:t -*-
;;
;; Copyright (C) 2026 Eamonnn Sullivan
;;
;;
;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Version: 1.0
;; Keywords: emacs email testing
;; URL: https://eamonnsullivan.co.uk
;;
;;
;;; Commentary:
;;
;;
;;; Code:

(require 'eds-email)

(describe "eds-email/get-from-field"
  (it "gets the 'from' field from a message"
    (expect (eds-email/get-from-field '(:from ((:name "User" :email "user@example.com"))))
            :to-equal '((:name "User" :email "user@example.com"))))
  (it "returns nil when 'from' field is absent"
    (expect (eds-email/get-from-field '(:subject "Test Subject" :date "2024-06-01"))
            :to-equal nil)))

(describe "eds-email/get-contact-email"
  (it "returns the email address part of a contact"
    (expect (eds-email/get-contact-email '(:email "user@example.com" :name "User"))
            :to-equal "user@example.com"))
  (it "returns nil when no email is present"
    (expect (eds-email/get-contact-email '(:name "User"))
            :to-be nil)))

(describe "eds-email/get-mu4e-from-string-search"
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
      (expect (eds-email/get-mu4e-from-search-string msg)
              :to-equal "from:niko@example.com"))))

(describe "eds-email/get-subject-from-msg"
  (it "returns the subject"
    (expect (eds-email/get-subject-from-msg '(:subject "Test Subject"))
            :to-equal "Test Subject"))
  (it "returns \"No Subject\" when no subject is present"
    (expect (eds-email/get-subject-from-msg '(:something "else"))
            :to-equal "No Subject")))

(describe "eds-email/get-sendmail-extra-args"
  (it "handles the eamonn.sullivan gmail account"
    (expect (eds-email/get-sendmail-extra-args "eamonn.sullivan@gmail.com")
            :to-equal '("-a" "gmail-eamonn")))
  (it "handles the svp account"
    (expect (eds-email/get-sendmail-extra-args "svpsouthruislip@gmail.com")
            :to-equal '("-a" "gmail-svp")))
  (it "handles the fastmail account"
    (expect (eds-email/get-sendmail-extra-args "me@eamonnsullivan.co.uk")
            :to-equal '("-a" "fastmail")))
  (it "handles other emails send to fastmail"
    (expect (eds-email/get-sendmail-extra-args "something@eamonnsullivan.co.uk")
            :to-equal '("-a" "fastmail")))
  (it "handles the SVP account on fastmail"
    (expect (eds-email/get-sendmail-extra-args "svp@svpsouthruislip.org.uk")
            :to-equal '("-a" "svp")))
  (it "returns nil for unknown email addresses"
    (expect (eds-email/get-sendmail-extra-args "user@example.com")
            :to-be nil)))

(provide 'test-eds-email)
;;; test-eds-email.el ends here
