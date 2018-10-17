;;; -*- lexical-binding: t -*-
;;; init-hydra.el --- hydra modes

(require 'init-global-behaviour)
(require 'eds)

(use-package hydra
  :ensure t)

(global-set-key
 (kbd "C-z")
 (defhydra "hydra-global-menu" (:color red
                                :hint nil)
   "
^Display^        ^Buffers^                    ^Actions^
^^^^^^^^^-----------------------------------------------------
_g_: zoom-in     _d_: close all buffers       _u_: upgrade all packages
_s_: zoom-out    _o_: open buffer on desktop  _r_: restart emacs

_q_: quit this menu
"
   ("g" default-text-scale-increase)
   ("s" default-text-scale-decrease)
   ("d" kill-all-buffers)
   ("r" stop-and-restart-emacs)
   ("u" upgrade-packages)
   ("o" eds/open-buffer-on-desktop)
   ("q" nil)))
(hydra-set-property 'hydra-global-menu :verbosity 1)

(provide 'init-hydra)
