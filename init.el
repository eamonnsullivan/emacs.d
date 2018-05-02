;;; init.el --- Initialization code for emacs

;; Copyright (c) 2018 Eamonn Sullivan

;; Author: Eamonn Sullivan <eamonn.sullivan@gmail.com>
;; Maintainer: Eamonn Sullivan <eamonn.sullivan@gmail.com>
;; Created 23 March 2017
;; Updated pretty much constantly

;; Homepage: https://github.com/eamonnsullivan/emacs.d

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;;; Code:

(prefer-coding-system 'utf-8-unix)
(set-language-environment "UTF-8")

;; packages
(setq custom-file (make-temp-file "emacs-custom")) ;; try this
(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/")
                    ("elpy" . "http://jorgenschaefer.github.io/packages/"))
 package-archive-priorities '(("melpa" . 1)))
(if (version< emacs-version "27.0")
    (package-initialize))

(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(setq use-package-always-ensure t)

;; useful global macros and functions
(setq load-prefer-newer t) ;; load newest of byte-compiled/text

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

(dolist (i '("org.el"
             "company.el"
             "general_utils.el"
             "proxy_load.el"
             "set_environment.el"
             "platform.el"
             "appearance.el"
             "global_key_bindings.el"
             "global_behaviour_settings.el"
             "helm_mode.el"
             "programming.el"
             "abbrev_mode.el"
             "ansi_term.el"
             "flycheck.el"
             "multiple-cursors.el"
             "projectile.el"
             "lastpass.el"
             "elfeed.el"
             "markdown.el"
             "server.el"))
  (load-user-file i))

;;; init.el ends here
