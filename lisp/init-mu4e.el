;;; -*- lexical-binding: t -*-
;;; mu4e.el -- provide mu4e for email

(use-package mu4e
  :straight
  (:local-repo "/opt/homebrew/share/emacs/site-lisp/mu/mu4e"
               :type built-in)
  :commands (mu4e)
  :config
  (setq mu4e-mu-binary (executable-find "mu")
        mu4e-maildir "~/.maildir"
        mu4e-get-mail-command (executable-find "offlineimap")
        mu4e-update-interval 300
        mu4e-attachment-dir "~/Downloads"
        mu4e-change-filenames-when-moving t
        mu4e-user-mail-address-list '("eamonn.sullivan@gmail.com")
        mu4e-maildir-shortcuts '( (:maildir "/gmail-eamonn/INBOX"              :key ?i)
                                  (:maildir "/gmail-eamonn/[Gmail].Sent Mail"  :key ?s)
                                  (:maildir "/gmail-eamonn/[Gmail].Trash"      :key ?t)
                                  (:maildir "/gmail-eamonn/[Gmail].All Mail"   :key ?a))
        mu4e-use-fancy-chars t
        mu4e-view-show-images t
        user-mail-address "eamonn.sullivan@gmail.com"
        user-full-name "Eamonn Sullivan"
        send-mail-function 'sendmail-send-it
        message-send-mail-function 'sendmail-send-it
        message-kill-buffer-on-exit t
        sendmail-program (executable-find "msmtp")
        message-sendmail-envelope-from 'header
        mail-user-agent 'mu4e-user-agent)
  (add-to-list 'mu4e-bookmarks '(:query "maildir:/gmail-eamonn/INBOX" :name "Inbox" :key ?i :favorite t))
  (add-hook 'mu4e-compose-mode-hook 'company-mode)

  (require 'epa-file)
  (epa-file-enable)
  (setq epa-pinentry-mode 'loopback)
  (auth-source-forget-all-cached))

(provide 'init-mu4e)
