;;; eds-email.el --- Email management and initialisation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan <me@eamonnsullivan.co.uk>

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Maintainer: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Created: 2026-02-27
;; Version: 0.1
;; Package-Requires: ((emacs "31.0"))
;; Keywords: email, communication, tools
;; URL: https://github.com/eamonnsullivan/eds-email

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides initialisation and configuration for email management
;; in Emacs, supporting workflows for sending, receiving, and organising messages.

;;; Licence:

;; This programme is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public Licence as published by
;; the Free Software Foundation, either version 3 of the Licence, or
;; (at your option) any later version.

;; This programme is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public Licence for more details.

;; You should have received a copy of the GNU General Public Licence
;; along with this programme.  If not, see <https://www.gnu.org/licenses/>.

;;; eds-email.el ends here

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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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
