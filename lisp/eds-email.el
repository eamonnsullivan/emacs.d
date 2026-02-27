;;; eds-email.el --- Library of small functions for dealing with email  -*- lexical-binding:t -*-
;;
;; Copyright (C) 2026 Eamonnn Sullivan
;;
;;
;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Version: 1.0
;; Keywords: emacs email
;; URL: https://eamonnsullivan.co.uk
;;
;;
;;; Commentary:
;;
;;
;;; Code:

(defun eds-email/get-subject-from-msg (msg)
  "Return the subject of MSG as a string, or \"No Subject\" if there isn't a subject."
  (or (plist-get msg :subject) "No Subject"))

(defun eds-email/get-from-field (msg)
  "Return the \"from\" field of MSG as a string."
  (plist-get msg :from))

(defun eds-email/get-contact-email (contact)
  "Extract the email from CONTACT."
  (plist-get contact :email))

(defun eds-email/get-mu4e-from-search-string (msg)
  "Return a mu4e search string for the \"from\" field of MSG."
  (let* ((from-field (eds-email/get-from-field msg))
         (contact (car from-field))
         (email (eds-email/get-contact-email contact)))
    (concat "from:" email)))

(defun eds-email/get-sendmail-extra-args (from-email)
  "Get the extra args we need to send an email from FROM-EMAIL."
  (let ((account
         (cond
          ((string-match "eamonn.sullivan@gmail.com" from-email) "gmail-eamonn")
          ((string-match "svpsouthruislip@gmail.com" from-email) "gmail-svp")
          ((string-match "eamonnsullivan.co.uk" from-email) "fastmail")
          ((string-match "svp@svpsouthruislip.org.uk" from-email) "svp"))))
    (if account
        (list "-a" account)
      nil)))

(defun eds-email/set-msmtp-account ()
  "Set the msmtp account based on the current from."
  (if (message-mail-p)
      (save-excursion
        (let*
            ((from (save-excursion
                     (message-narrow-to-headers)
                     (message-fetch-field "From")))
             (args (eds-email/get-sendmail-extra-args from)))
          (setopt message-sendmail-extra-arguments args)))))

(defun eds-email/from-search (msg)
  "Return a search string for the sender of MSG."
  (mu4e-search (eds-email/get-mu4e-from-search-string msg)))

(defun eds-email/msg-contact-matches (msg email)
  "Return t if MSG matches EMAIL in to, cc, or bcc."
  (or (mu4e-message-contact-field-matches msg :to email)
      (mu4e-message-contact-field-matches msg :cc email)
      (mu4e-message-contact-field-matches msg :bcc email)))

(provide 'eds-email)
;;; eds-email.el ends here
