;;; -*- lexical-binding: t -*-
;;; init-server.el -- starting/stopping the emacs server

;; server
(require 'server)
(add-hook 'after-init-hook (lambda ()
                             (unless (or (daemonp) (server-running-p))
                               (progn
                                 (setopt server-use-tcp t)
                                 (server-start)
                                 (require 'org-protocol)
                                 ;; apply my display preferences to the first frame
                                 (my-appearance-settings t))
                               (setopt server-raise-frame t))))

(provide 'init-server)
