;;; -*- lexical-binding: t -*-
;;; mu4e.el -- provide mu4e for email

(defun eds/from-search (msg)
  "Return a search string for the sender of MSG."
  (mu4e-search (eds/get-mu4e-from-search-string msg)))

(defun eds/set-msmtp-account ()
  "Set the msmtp account based on the current from."
  (if (message-mail-p)
      (save-excursion
        (let*
            ((from (save-excursion
                     (message-narrow-to-headers)
                     (message-fetch-field "From")))
             (args (eds/get-sendmail-extra-args from)))
          (setq message-sendmail-extra-arguments args)))))

(use-package mu4e
  :straight
  (:local-repo "/opt/homebrew/share/emacs/site-lisp/mu/mu4e"
               :type built-in)
  :commands (mu4e)
  :config
  (require 'eds)
  (setq mu4e-mu-binary (executable-find "mu")
        mu4e-maildir "~/.maildir"
        mu4e-get-mail-command "~/bin/sync-mailboxes.sh"
        mu4e-update-interval 300
        mu4e-attachment-dir (expand-file-name "~/Downloads")
        mu4e-change-filenames-when-moving t
        mu4e-search-skip-duplicates t
        mu4e-search-include-related nil
        mu4e-compose-complete-only-personal t
        mu4e-compose-complete-only-after "2020-01-01"
        mu4e-sent-messages-behavior
        (lambda ()
          (if (or (string= (message-sendmail-envelope-from) "eamonn.sullivan@gmail.com")
                  (string= (message-sendmail-envelope-from) "svpsouthruislip@gmail.com"))
              'delete 'sent))
        mu4e-user-mail-address-list '("me@eamonnsullivan.co.uk" "eamonn.sullivan@gmail.com" "svpsouthruislip@gmail.com")
        mu4e-maildir-shortcuts '( (:maildir "/fastmail/INBOX"                  :key ?i)
                                  (:maildir "/fastmail/Sent"                   :key ?s)
                                  (:maildir "/fastmail/Archive"                :key ?a)
                                  (:maildir "/gmail-eamonn/INBOX"              :key ?g)
                                  (:maildir "/gmail-eamonn/[Gmail].Sent Mail"  :key ?e)
                                  (:maildir "/gmail-eamonn/[Gmail].All Mail"   :key ?r)
                                  (:maildir "/gmail-svp/INBOX"                 :key ?v)
                                  (:maildir "/gmail-svp/[Gmail].Sent Mail"     :key ?x)
                                  (:maildir "/gmail-svp/[Gmail].All Mail"      :key ?p))
        mu4e-bookmarks '((:name "Inbox"
                                :query "maildir:/fastmail/INBOX or maildir:/gmail-eamonn/INBOX or maildir:/gmail-svp/INBOX"
                                :key ?i
                                :favorite t)
                         (:name "Sent"
                                :query "maildir:/fastmail/Sent or maildir:/gmail-eamonn/[Gmail].Sent Mail or maildir:/gmail-svp/[Gmail].Sent Mail"
                                :key ?s)
                         (:name "Unread"
                                :query "flag:unread and (maildir:\"/gmail-eamonn/[Gmail].All Mail\" OR maildir:\"/gmail-svp/[Gmail].All Mail\" OR maildir:/fastmail/*)"
                                :key ?u)
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
                              (mu4e-message-contact-field-matches msg :to "eamonnsullivan.co.uk")))
                          :vars
                          '((user-mail-address . "me@eamonnsullivan.co.uk")
                            (user-full-name . "Eamonn Sullivan")
                            (mu4e-drafts-folder . "/fastmail/Drafts")
                            (mu4e-sent-folder . "/fastmail/Sent")
                            (mu4e-trash-folder . "/fastmail/Trash")
                            (mu4e-refile-folder . "/fastmail/Archive")))

                        ,(make-mu4e-context
                          :name "gmail"
                          :enter-func
                          (lambda () (mu4e-message "Entering Gmail context"))
                          :leave-func
                          (lambda () (mu4e-message "Leaving Gmail context"))
                          :match-func
                          (lambda (msg)
                            (when msg
                              (or (mu4e-message-contact-field-matches msg :to "eamonn.sullivan@gmail.com")
                                  (mu4e-message-contact-field-matches msg :cc "eamonn.sullivan@gmail.com"))))
                          :vars
                          '((user-mail-address . "eamonn.sullivan@gmail.com")
                            (user-full-name . "Eamonn Sullivan")
                            (mu4e-drafts-folder . "/gmail-eamonn/[Gmail].Drafts")
                            (mu4e-sent-folder . "/gmail-eamonn/[Gmail].Sent Mail")
                            (mu4e-trash-folder . "/gmail-eamonn/[Gmail].Trash")
                            (mu4e-refile-folder . "/gmail-eamonn/[Gmail].All Mail")))

                        ,(make-mu4e-context
                          :name "SVP"
                          :enter-func
                          (lambda () (mu4e-message "Entering SVP context"))
                          :leave-func
                          (lambda () (mu4e-message "Leaving SVP context"))
                          :match-func
                          (lambda (msg)
                            (when msg
                              (or (mu4e-message-contact-field-matches msg :to "svpsouthruislip@gmail.com")
                                  (mu4e-message-contact-field-matches msg :to "svpsouthruislip.org.uk"))))
                          :vars
                          '((user-mail-address . "svpsouthruislip@gmail.com")
                            (user-full-name . "SVP South Ruislip")
                            (mu4e-drafts-folder . "/gmail-svp/[Gmail].Drafts")
                            (mu4e-sent-folder . "/gmail-svp/[Gmail].Sent Mail")
                            (mu4e-trash-folder . "/gmail-svp/[Gmail].Trash")
                            (mu4e-refile-folder . "/gmail-svp/[Gmail].All Mail"))))
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
               '("Search for sender" . eds/from-search) t)
  (add-to-list 'mu4e-view-actions
               '("Org" . eds/orgify-msg) t)
  (add-to-list 'mu4e-view-actions
               '("TODO" . eds/create-todo-from-email) t)

  (require 'epa-file)
  (epa-file-enable)
  (setq epa-pinentry-mode 'loopback)
  (auth-source-forget-all-cached))

(provide 'init-mu4e)
