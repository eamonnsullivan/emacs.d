(use-package helm-slack
  :straight
  (helm-slack :type git :host github :repo "yuya373/helm-slack")
  :after (slack)
  :bind (("<f6>" . helm-slack)))

(use-package slack
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t)
  (setq slack-prefer-current-team t)
  :config
  (slack-register-team
   :name "BBC Digital Publishing"
   :token (auth-source-pick-first-password
           :host "bbcdigitalpublishing.slack.com"
           :user "eamonn.sullivan@bbc.co.uk"))
  (slack-register-team
   :name "Clojurians"
   :token (auth-source-pick-first-password
           :host "clojurians.slack.com"
           :user "eamonn.sullivan@bbc.co.uk")))

(use-package alert
  :commands (alert)
  :custom
  (alert-default-style 'libnotify))

(provide 'init-slack)
