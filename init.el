;;; init.el --- Initialization code for emacs

;; Copyright (c) 2017 Eamonn Sullivan

;; Author: Eamonn Sullivan <eamonn.sullivan@gmail.com>
;; Maintainer: Eamonn Sullivan <eamonn.sullivan@gmail.com>
;; Created 23 March 2017

;; Homepage: https://github.com/eamonnsullivan/emacs.d

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;;; Code:

;; packages
(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("elpy" . "http://jorgenschaefer.github.io/packages/"))
 package-archive-priorities '(("melpa" . 1)))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)
(setq use-package-always-ensure t)

;; useful global macros
(defmacro when-available (func foo)
  "*Do something if FUNCTION is available."
  `(when (fboundp ,func) ,foo))

;; The rest of my init file, broken up into modules
(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

(load-user-file "general_utils.el")
(load-user-file "proxy_load.el")
(load-user-file "set_environment.el")
(load-user-file "platform.el")
(load-user-file "appearance.el")
(load-user-file "global_key_bindings.el")
(load-user-file "global_behaviour_settings.el")
(load-user-file "helm_mode.el")
(load-user-file "programming.el")
(load-user-file "abbrev_mode.el")
(load-user-file "ansi_term.el")
(load-user-file "org.el")
(load-user-file "flycheck.el")
(load-user-file "multiple-cursors.el")
(load-user-file "lastpass.el")
(load-user-file "projectile.el")

;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("f5512c02e0a6887e987a816918b7a684d558716262ac7ee2dd0437ab913eaec6" "d9129a8d924c4254607b5ded46350d68cc00b6e38c39fc137c3cfb7506702c12" default)))
 '(describe-char-unidata-list
   (quote
    (name old-name general-category canonical-combining-class bidi-class decomposition decimal-digit-value digit-value numeric-value iso-10646-comment)))
 '(fci-rule-color "#383838")
 '(ispell-program-name "/usr/local/bin/aspell")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-agenda-files
   (quote
    ("~/Dropbox/org/notes.org" "~/Dropbox/org/svp.org" "~/Dropbox/org/recipes.org" "~/Dropbox/org/tasks.org" "~/Dropbox/org/ideas.org")))
 '(package-selected-packages
   (quote
    (zenburn-theme dracula-theme elpy s org-plus-contrib undo-tree mc-extras multiple-cursors helm-ag org org-projectile exec-path-from-shell web-mode flycheck eclimd json-mode sql-upcase groovy-mode helm-projectile projectile eldoc-eval smartparens helm-ls-git use-package editorconfig js2-mode feature-mode package+ magit helm ensime)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(scala-indent:align-forms nil)
 '(scala-indent:align-parameters t)
 '(scala-indent:default-run-on-strategy 2)
 '(scala-indent:indent-value-expression t)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
