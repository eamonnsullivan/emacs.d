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
        mu4e-attachment-dir "~/Downloads"
        mu4e-change-filenames-when-moving t
        mu4e-search-skip-duplicates t
        mu4e-search-include-related nil
        mu4e-sent-messages-behavior 'delete
        mu4e-user-mail-address-list '("eamonn.sullivan@gmail.com" "svpsouthruislip@gmail.com")
        mu4e-maildir-shortcuts '( (:maildir "/gmail-eamonn/INBOX"              :key ?i)
                                  (:maildir "/gmail-eamonn/[Gmail].Sent Mail"  :key ?s)
                                  (:maildir "/gmail-eamonn/[Gmail].Trash"      :key ?t)
                                  (:maildir "/gmail-eamonn/[Gmail].All Mail"   :key ?a)
                                  (:maildir "/gmail-eamonn/[Gmail].Starred"    :key ?z)
                                  (:maildir "/gmail-svp/INBOX"                 :key ?v)
                                  (:maildir "/gmail-svp/[Gmail].All Mail"      :key ?p)
                                  (:maildir "/gmail-svp/[Gmail].Sent Mail"     :key ?x))
        mu4e-bookmarks '((:name "Inbox"
                                :query "maildir:/gmail-eamonn/INBOX"
                                :key ?i
                                :favorite t)
                         (:name "Important"
                                :query "maildir:/gmail-eamonn/[Gmail].Important"
                                :key ?p)
                         (:name "Unread messages"
                                :query "flag:unread AND NOT maildir:/gmail-eamonn/[Gmail].Spam"
                                :key ?u)
                         (:name "SVP Inbox"
                                :query "maildir:/gmail-svp/INBOX"
                                :key ?v)
                         (:name "SVP All Mail"
                                :query "maildir:/gmail-svp/[Gmail].All Mail"
                                :key ?p)
                         (:name "To Hillingdon Foodbank"
                                :query "to:Hillingdon Foodbank"
                                :key ?h)
                         (:name "Food bank"
                                :query "to:foodbank@svpsouthruislip.org.uk"
                                :key ?f)
                         (:name "SVP Info"
                                :query "maildir:/gmail-svp/svp-info"
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
                            (mu4e-trash-folder . "/gmail-svp/[Gmail].Trash"))))
        mu4e-context-policy 'pick-first
        mu4e-compose-context-policy 'ask
        mu4e-use-fancy-chars t
        mu4e-view-show-images t
        send-mail-function 'message-send-mail-with-sendmail
        message-send-mail-function 'message-send-mail-with-sendmail
        message-kill-buffer-on-exit t
        sendmail-program (executable-find "msmtp")
        message-sendmail-envelope-from 'header
        mail-user-agent 'mu4e-user-agent)
  (add-hook 'mu4e-compose-mode-hook 'company-mode)
  (add-hook 'message-send-mail-hook 'eds/set-msmtp-account)
  (add-to-list 'mu4e-view-actions
               '("ViewInBrowser" . mu4e-action-view-in-browser) t)

  (require 'epa-file)
  (epa-file-enable)
  (setq epa-pinentry-mode 'loopback)
  (auth-source-forget-all-cached))

(provide 'init-mu4e)
