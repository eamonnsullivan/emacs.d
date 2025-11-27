;;; -*- lexical-binding: t -*-
;;; mu4e.el -- provide mu4e for email

(use-package mu4e
  :straight
  (:local-repo "/opt/homebrew/share/emacs/site-lisp/mu/mu4e"
               :type built-in)
  :commands (mu4e)
  :config
  (require 'eds)
  (setq mu4e-mu-binary (executable-find "mu")
        mu4e-maildir "~/.maildir"
        mu4e-get-mail-command (executable-find "offlineimap")
        mu4e-update-interval 300
        mu4e-attachment-dir (expand-file-name "~/Downloads")
        mu4e-change-filenames-when-moving t
        mu4e-search-skip-duplicates t
        mu4e-search-include-related nil
        mu4e-sent-messages-behavior
        (lambda ()
          (if (or (string= (message-sendmail-envelope-from) "eamonn.sullivan@gmail.com")
                  (string= (message-sendmail-envelope-from) "svpsouthruislip@gmail.com"))
              'delete 'sent))
        mu4e-user-mail-address-list '("eamonn.sullivan@gmail.com" "svpsouthruislip@gmail.com" "me@eamonnsullivan.co.uk")
        mu4e-maildir-shortcuts '( (:maildir "/gmail-eamonn/INBOX"              :key ?i)
                                  (:maildir "/gmail-eamonn/[Gmail].Sent Mail"  :key ?s)
                                  (:maildir "/gmail-eamonn/[Gmail].All Mail"   :key ?a)
                                  (:maildir "/gmail-eamonn/[Gmail].Starred"    :key ?z)
                                  (:maildir "/fastmail/INBOX"                  :key ?t)
                                  (:maildir "/fastmail/Sent"                   :key ?e)
                                  (:maildir "/gmail-svp/INBOX"                 :key ?v)
                                  (:maildir "/gmail-svp/[Gmail].All Mail"      :key ?p)
                                  (:maildir "/gmail-svp/[Gmail].Sent Mail"     :key ?x))
        mu4e-bookmarks '((:name "Inbox"
                                :query "maildir:/gmail-eamonn/INBOX"
                                :key ?i
                                :favorite t)
                         (:name "Fastmail"
                                :query "maildir:/fastmail/INBOX"
                                :key ?t)
                         (:name "Fastmail Sent"
                                :query "maildir:/fastmail/Sent"
                                :key ?e)
                         (:name "Unread"
                                :query "maildir:\"/gmail-eamonn/[Gmail].All Mail\" AND flag:unread"
                                :key ?u)
                         (:name "Github"
                                :query "maildir:\"/gmail-eamonn/[Gmail].All Mail\" AND tags:ads/github"
                                :key ?g)
                         (:name "SVP Inbox"
                                :query "maildir:/gmail-svp/INBOX"
                                :key ?v)
                         (:name "Food bank"
                                :query "to:foodbank@svpsouthruislip.org.uk"
                                :key ?f)
                         (:name "SVP Info"
                                :query "to:info@svpsouthruislip.org.uk"
                                :key ?n))
        mu4e-contexts `(,(make-mu4e-context
                          :name "personal"
                          :enter-func
                          (lambda () (mu4e-message "Entering personal context"))
                          :leave-func
                          (lambda () (mu4e-message "Leaving personal context"))
                          :match-func
                          (lambda (msg)
                            (when msg
                              (mu4e-message-contact-field-matches msg :to "eamonn.sullivan@gmail.com")))
                          :vars
                          '((user-mail-address . "eamonn.sullivan@gmail.com")
                            (user-full-name . "Eamonn Sullivan")
                            (mu4e-drafts-folder . "/gmail-eamonn/[Gmail].Drafts")
                            (mu4e-sent-folder . "/gmail-eamonn/[Gmail].Sent Mail")
                            (mu4e-trash-folder . "/gmail-eamonn/[Gmail].Trash")))

                        ,(make-mu4e-context
                          :name "SVP"
                          :enter-func
                          (lambda () (mu4e-message "Entering SVP context"))
                          :leave-func
                          (lambda () (mu4e-message "Leaving SVP context"))
                          :match-func
                          (lambda (msg)
                            (when msg
                              (mu4e-message-contact-field-matches msg :to "svpsouthruislip@gmail.com")))
                          :vars
                          '((user-mail-address . "svpsouthruislip@gmail.com")
                            (user-full-name . "SVP South Ruislip")
                            (mu4e-drafts-folder . "/gmail-svp/[Gmail].Drafts")
                            (mu4e-sent-folder . "/gmail-svp/[Gmail].Sent Mail")
                            (mu4e-trash-folder . "/gmail-svp/[Gmail].Trash")))

                        ,(make-mu4e-context
                          :name "Fastmail"
                          :enter-func
                          (lambda () (mu4e-message "Entering Fastmail context"))
                          :leave-func
                          (lambda () (mu4e-message "Leaving Fastmail context"))
                          :match-func
                          (lambda (msg)
                            (when msg
                              (mu4e-message-contact-field-matches msg :to "me@eamonnsullivan.co.uk")))
                          :vars
                          '((user-mail-address . "me@eamonnsullivan.co.uk")
                            (user-full-name . "Eamonn Sullivan")
                            (mu4e-drafts-folder . "/fastmail/Drafts")
                            (mu4e-sent-folder . "/fastmail/Sent")
                            (mu4e-trash-folder . "/fastmail/Trash"))))
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
  (add-hook 'mu4e-compose-mode-hook 'company-mode)
  (add-hook 'message-send-mail-hook 'eds/set-msmtp-account)
  (add-to-list 'mu4e-view-actions
               '("Search for sender" . eds/other-messages-from-sender) t)
  (add-to-list 'mu4e-view-actions
               '("Org" . eds/orgify-msg) t)

  (require 'epa-file)
  (epa-file-enable)
  (setq epa-pinentry-mode 'loopback)
  (auth-source-forget-all-cached))

(provide 'init-mu4e)
