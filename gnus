(setq user-mail-address "eamonn.sullivan@gmail.com"
      user-full-name "Eamonn Sullivan")

(setq gnus-select-method
      '(nnimap "gmail"
	       (nnimap-address "imap.gmail.com")
	       (nnimap-server-port "imaps")
	       (nnimap-stream ssl)))

(setq smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(defun my-message-mode-setup ()
  (setq fill-column 72)
  (turn-on-auto-fill))
(add-hook 'message-mode-hook 'my-message-mode-setup)

(setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject)
(setq gnus-thread-hide-subtree t)
(setq gnus-thread-ignore-subject t)
