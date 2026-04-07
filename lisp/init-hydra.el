;;; init-hydra.el --- Hydra mode initialisation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan <me@eamonnsullivan.co.uk>

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Maintainer: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Created: 2018-05-05
;; Version: 0.1
;; Package-Requires: ((emacs "24.4"))
;; Keywords: hydra, bindings, convenience, tools
;; URL: https://github.com/eamonnsullivan/init-hydra

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides initialisation and configuration for Hydra mode,
;; enabling powerful key binding management and workflow enhancements in Emacs.

;;; Licence:

;; This programme is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public Licence as published by
;; the Free Software Foundation, either version 3 of the Licence, or
;; (at your option) any later version.

;; This programme is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public Licence for more details.

;; You should have received a copy of the GNU General Public Licence
;; along with this programme.  If not, see <https://www.gnu.org/licenses/>.

(straight-use-package 'hydra)

(require 'init-global-behaviour)
(require 'eds-blog)
(require 'eds-utils)

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

(defun eds/reset-face-height ()
  "Reset the default face height to the default value."
  (interactive)
  (global-text-scale-adjust--default-height (face-attribute 'default :height)))

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
   ("g" text-scale-increase)
   ("s" text-scale-decrease)
   ("d" kill-all-buffers)
   ("l" global-display-line-numbers-mode)
   ("r" stop-and-restart-emacs)
   ("u" eds-straight-pull-or-prune)
   ("o" eds-utils/open-buffer-on-desktop)
   ("q" nil)))

(hydra-set-property 'hydra-global-menu :verbosity 1)

(global-set-key
 (kbd "<f5>")
 (defhydra hydra-web-work (:color blue :hint nil)
   "
_c_reate new personal blog post | _C_reate new SVP blog post | _q_uit |
"
   ("c" eds-blog/start-personal-blog-post)
   ("C" eds-blog/start-svp-blog-post)
   ("q" nil)))

(provide 'init-hydra)
;;; init-hydra.el ends here
