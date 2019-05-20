(use-package slack
  :ensure t
  :secret
  (slack-start "~/work.el")
  :commands
  (slack-start)
  :custom
  (slack-buffer-emojify t)
  (slack-prefer-current-team t)
  (slack-redirect-url "http://localhost:8081"))

(use-package alert
  :ensure t
  :commands (alert)
  :custom
  (alert-default-style 'libnotify))

(provide 'init-slack)
