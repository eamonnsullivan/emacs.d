;;; -*- lexical-binding: t -*-
;;; init-hydra.el --- hydra modes

(straight-use-package 'hydra)

(require 'init-global-behaviour)
(require 'eds)

(defvar hydra-stack nil)

(defun hydra-push (expr)
  (push `(lambda () ,expr) hydra-stack))

(defun hydra-pop ()
  (interactive)
  (let ((x (pop hydra-stack)))
    (when x
      (funcall x))))


;; from https://manuel-uberti.github.io//emacs/2019/11/02/thirty-straight-days/
(defun eds-straight-pull-or-prune (&optional prune)
  "Update all available packages via `straight'.
With PRUNE, prune the build cache and the build directory."
  (interactive "P")
  (if prune
      (when (y-or-n-p "Prune build cache and build directory?")
        (straight-prune-build-cache)
        (straight-prune-build-directory))
    (when (y-or-n-p "Update all available packages?")
      (straight-pull-all))))

(defhydra hydra-straight-helper (:hint nil)
  "
_c_heck all       |_f_etch all     |_m_erge all      |_n_ormalize all   |p_u_sh all
_C_heck package   |_F_etch package |_M_erge package  |_N_ormlize package|p_U_sh package
----------------^^+--------------^^+---------------^^+----------------^^+------------||_q_uit||
_r_ebuild all     |_p_ull all      |_v_ersions freeze|_w_atcher start   |_g_et recipe
_R_ebuild package |_P_ull package  |_V_ersions thaw  |_W_atcher quit    |prun_e_ build"
  ("c" straight-check-all)
  ("C" straight-check-package)
  ("r" straight-rebuild-all)
  ("R" straight-rebuild-package)
  ("f" straight-fetch-all)
  ("F" straight-fetch-package)
  ("p" straight-pull-all)
  ("P" straight-pull-package)
  ("m" straight-merge-all)
  ("M" straight-merge-package)
  ("n" straight-normalize-all)
  ("N" straight-normalize-package)
  ("u" straight-push-all)
  ("U" straight-push-package)
  ("v" straight-freeze-versions)
  ("V" straight-thaw-versions)
  ("w" straight-watcher-start)
  ("W" straight-watcher-quit)
  ("g" straight-get-recipe)
  ("e" straight-prune-build)
  ("q" nil))

(key-chord-define-global "QQ" 'hydra-straight-helper/body)

(global-set-key
 (kbd "C-z")
 (defhydra hydra-global-menu (:color red :hint nil)
   "
^Display^        ^Buffers^                    ^Actions^
^^^^^^^^^-----------------------------------------------------
_g_: zoom-in     _d_: close all buffers       _u_: update all packages
_s_: zoom-out    _o_: open buffer on desktop  _l_: display line numbers

_q_: quit this menu                         _r_: restart emacs
"
   ("g" default-text-scale-increase)
   ("s" default-text-scale-decrease)
   ("d" kill-all-buffers)
   ("l" global-display-line-numbers-mode)
   ("r" stop-and-restart-emacs)
   ("u" eds-straight-pull-or-prune)
   ("o" eds/open-buffer-on-desktop)
   ("q" nil)))

(hydra-set-property 'hydra-global-menu :verbosity 1)

(global-set-key
 (kbd "<f5>")
 (defhydra hydra-web-work (:color blue :hint nil)
   "
_c_reate new personal blog post | _C_reate new SVP blog post | _q_uit |
"
   ("c" eds/start-personal-blog-post)
   ("C" eds/start-svp-blog-post)
   ("q" nil)))

(provide 'init-hydra)
