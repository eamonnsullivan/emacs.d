;;; -*- lexical-binding: t -*-
;;; init-hydra.el --- hydra modes

(require 'init-global-behaviour)
(require 'init-kbd-macros)

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

(defvar eds/javascript-macros
  (defhydra "hydra-my-javascript-macros" (:color blue)
      ("t" (execute-kbd-macro (symbol-function 'insert-enzyme-test-case)) "Insert an enzyme test case")
      ("q" nil "quit")))

(eval-after-load 'js2-mode
  '(key-chord-define js2-mode-map "MM" eds/javascript-macros))

(provide 'init-hydra)
