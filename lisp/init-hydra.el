;;; -*- lexical-binding: t -*-
;;; init-hydra.el --- hydra modes

(require 'init-global-behaviour)

(use-package hydra
  :ensure t)

(global-set-key
 (kbd "C-z")
 (defhydra "hydra-global-menu" (:color red)
   "My Global Menu"
   ("g" default-text-scale-increase "zoom-in")
   ("s" default-text-scale-decrease "zoom-out")
   ("d" kill-all-buffers "kill all buffers")
   ("r" stop-and-restart-emacs "stop and restart emacs")
   ("u" upgrade-packages "upgrade all packages")
   ("q" nil "quit")))
(hydra-set-property 'hydra-global-menu :verbosity 1)

(provide 'init-hydra)
