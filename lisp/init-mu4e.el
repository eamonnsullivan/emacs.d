;;; init-mu4e.el --- Mu4e initialisation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan <me@eamonnsullivan.co.uk>

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Maintainer: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Created: 2025-11-06
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: mu4e, email, communication, tools
;; URL: https://github.com/eamonnsullivan/init-mu4e

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides initialisation and configuration for mu4e,
;; enabling advanced email management and workflow enhancements in Emacs.

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

;;; Code:

(use-package mu4e
  :straight (:type built-in)
  :load-path
  (lambda ()
    (list (if (eq system-type 'gnu/linux)
              "/home/linuxbrew/.linuxbrew/share/emacs/site-lisp/mu/mu4e"
            "/opt/homebrew/share/emacs/site-lisp/mu/mu4e")))
  :commands (mu4e)
  :config
  (require 'eds-email)
  (setopt mu4e-mu-binary (executable-find "mu")
        mu4e-maildir "~/.maildir"
        mu4e-get-mail-command "~/bin/sync-mailboxes.sh"
        mu4e-update-interval 300
        mu4e-attachment-dir (expand-file-name "~/Downloads")
        mu4e-change-filenames-when-moving t
        mu4e-search-skip-duplicates t
        mu4e-search-include-related nil
        mu4e-compose-complete-only-personal t
        mu4e-compose-complete-only-after "2020-01-01"
        mu4e-user-mail-address-list '("me@eamonnsullivan.co.uk" "svp@svpsouthruislip.org.uk")
        mu4e-maildir-shortcuts '( (:maildir "/fastmail/INBOX"                  :key ?i)
                                  (:maildir "/fastmail/Sent"                   :key ?s)
                                  (:maildir "/SVP/INBOX"                       :key ?V)
                                  (:maildir "/SVP/Sent"                        :key ?S))
        mu4e-bookmarks '((:name "Inbox"
                                :query "maildir:/fastmail/INBOX or maildir:/SVP/INBOX"
                                :key ?i
                                :favorite t)
                         (:name "Sent"
                                :query "maildir:/fastmail/Sent or maildir:/SVP/Sent"
                                :key ?s)
                         (:name "Unread"
                                :query "flag:unread AND NOT (maildir:/fastmail/Spam OR maildir:/SVP/Spam OR flag:trashed)"
                                :key ?u)
                         (:name "Archived"
                                :query "maildir:/fastmail/Archive OR maildir:/SVP/Archive"
                                :key ?a)
                         (:name "Github"
                                :query "from:github.com"
                                :key ?h)
                         (:name "Food bank"
                                :query "to:foodbank@svpsouthruislip.org.uk or cc:foodbank@svpsouthruislip.org.uk"
                                :key ?f)
                         (:name "SVP Info"
                                :query "to:info@svpsouthruislip.org.uk or cc:info@svpsouthruislip.org.uk"
                                :key ?n)
                         (:name "Backups"
                                :query "to:backup@eamonnsullivan.co.uk"
                                :key ?b))
        mu4e-contexts `(,(make-mu4e-context
                          :name "personal"
                          :enter-func
                          (lambda () (mu4e-message "Entering personal context"))
                          :leave-func
                          (lambda () (mu4e-message "Leaving personal context"))
                          :match-func
                          (lambda (msg)
                            (when msg
                              (eds-email/msg-contact-matches msg "eamonnsullivan.co.uk")))
                          :vars
                          '((user-mail-address . "me@eamonnsullivan.co.uk")
                            (user-full-name . "Eamonn Sullivan")
                            (mu4e-drafts-folder . "/fastmail/Drafts")
                            (mu4e-sent-folder . "/fastmail/Sent")
                            (mu4e-trash-folder . "/fastmail/Trash")
                            (mu4e-refile-folder . "/fastmail/Archive")))

                        ,(make-mu4e-context
                          :name "SVP"
                          :enter-func
                          (lambda () (mu4e-message "Entering SVP context"))
                          :leave-func
                          (lambda () (mu4e-message "Leaving SVP context"))
                          :match-func
                          (lambda (msg)
                            (when msg
                              (eds-email/msg-contact-matches msg "svpsouthruislip.org.uk")))
                          :vars
                          '((user-mail-address . "svp@svpsouthruislip.org.uk")
                            (user-full-name . "South Ruislip SVP")
                            (mu4e-drafts-folder . "/SVP/Drafts")
                            (mu4e-sent-folder . "/SVP/Sent")
                            (mu4e-trash-folder . "/SVP/Trash")
                            (mu4e-refile-folder . "/SVP/Archive"))))
        mu4e-context-policy 'pick-first
        mu4e-compose-context-policy 'ask
        mu4e-use-fancy-chars t
        mu4e-view-show-images t
        send-mail-function 'message-send-mail-with-sendmail
        message-send-mail-function 'message-send-mail-with-sendmail
        message-kill-buffer-on-exit t
        sendmail-program (executable-find "msmtp")
        message-sendmail-envelope-from 'header
        mail-user-agent 'mu4e-user-agent
        org-mu4e-link-query-in-headers-mode nil
        mu4e-headers-show-threads nil
        mu4e-hide-index-messages t
        mu4e-headers-include-related nil
        mu4e-headers-show-threads nil
        mu4e-confirm-quit nil)
  (add-hook 'mu4e-compose-mode-hook 'corfu-mode)
  (add-hook 'message-send-mail-hook 'eds-email/set-msmtp-account)
  (add-to-list 'mu4e-view-actions
               '("Search for sender" . eds-email/from-search) t)
  (add-to-list 'mu4e-view-actions
               '("Org" . eds-org/capture-email) t)
  (add-to-list 'mu4e-view-actions
               '("TODO" . eds-org/capture-email-todo) t)
  (require 'epa-file)
  (epa-file-enable)
  (setopt epa-pinentry-mode 'loopback)
  (auth-source-forget-all-cached))

(provide 'init-mu4e)
;;; init-mu4e.el ends here
