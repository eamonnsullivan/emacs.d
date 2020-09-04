(use-package emojify)

(use-package helm-slack
  :straight
  (helm-slack :type git :host github :repo "yuya373/helm-slack")
  :after (slack)
  :bind (("s-<f6>" . helm-slack-unreads)))

(use-package slack
  :commands (slack-start)
  :custom
  (slack-buffer-emojify t)
  (slack-thread-also-send-to-room nil)
  :config
  (slack-register-team
   :name "BBC Digital Publishing"
   :token (auth-source-pick-first-password
           :host "bbcdigitalpublishing.slack.com"
           :user "eamonn.sullivan@bbc.co.uk")
   :subscribed-channels '((engineering announce cop-innovation general random))
   :default t
   :modeline-enabled t)
  (slack-register-team
   :name "Clojurians"
   :token (auth-source-pick-first-password
           :host "clojurians.slack.com"
           :user "eamonn.sullivan@bbc.co.uk")
   :subscribed-channels '((clojure-uk clojure clojurescript))
   :default nil
   :visible-threads t))

(use-package alert
  :commands (alert)
  :custom
  (alert-default-style 'libnotify))

(global-set-key
 (kbd "<f6>")
 (defhydra slack-hydra (:color blue :hint none)
   "
channels                  message                 Global
---------------------------------------------------------------------------------
_u_: Unread channels      _e_: Edit message       _S_: Start slack
_a_: All channels         _r_: Add reaction       _C_: Close connections
_t_: All threads          _d_: Delete message     _q_: quit
_s_: Select channel       _m_: Mention
_p_: Slack users
_i_: Slack individual
"
   ("u" helm-slack-unreads)
   ("a" helm-slack)
   ("t" slack-all-threads)
   ("s" slack-select-rooms)
   ("p" slack-create-group)
   ("i" slack-im-select)
   ("e" slack-message-edit)
   ("r" slack-message-add-reaction)
   ("d" slack-message-delete)
   ("m" slack-message-embed-mention)
   ("S" slack-start)
   ("C" slack-ws-close)
   ("q" nil)))

(provide 'init-slack)
