;;; -*- lexical-binding: t -*-
;;; init.el --- Initialization code for emacs

;; Copyright (c) 2025 Eamonn Sullivan

;; Author: Eamonn Sullivan <eamonn.sullivan@gmail.com>
;; Maintainer: Eamonn Sullivan <eamonn.sullivan@gmail.com>
;; Updated pretty much constantly

;; Homepage: https://github.com/eamonnsullivan/emacs.d
;; Version: 1.0.0

;; Keywords: emacs config personal

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

;; (defvar comp-deferred-compilation-deny-list ())

(setq straight-use-package-by-default t
      straight-use-package-mode t
      straight-enable-use-package-integration t
      straight-disable-native-compile nil
      package-enable-at-startup nil
      straight-repository-branch "develop"
      straight-recipes-gnu-elpa-use-mirror t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'org)
(straight-use-package 'use-package)

(cond
 ((eq system-type 'gnu/linux)
  (setq straight-find-executable "/usr/bin/find"))
 ((eq system-type 'darwin)
  (setq straight-find-executable "/opt/homebrew/opt/findutils/libexec/gnubin/find")))

(setq load-prefer-newer t) ;; load newest of byte-compiled/text

(defmacro when-available (func foo)
  "*Do something if FUNCTION is available."
  `(when (fboundp ,func) ,foo))

(prefer-coding-system 'utf-8-unix)
(set-language-environment "UTF-8")

(setq custom-file (make-temp-file "emacs-custom")) ;; Disable the auto-generated customize section.

;; me
(setq user-full-name "Eamonn Sullivan")
(cond
 ((string= user-login-name "eamonn")
  (setq user-mail-address "eamonn.sullivan@gmail.com"))
 ((string= user-login-name "sullie09")
  (setq user-mail-address "eamonn.sullivan@bbc.co.uk")))

;; my location
(setq calendar-latitude 51.5)
(setq calendar-longitude -0.12)
(setq calendar-location-name "London, England")

(use-package vc
  :config
  (setq vc-follow-symlinks t))

(straight-use-package 'flymake)
(straight-use-package 'use-package)
(straight-use-package 'project)

;; The rest of my init file, broken up into libraries in the lisp directory
;; (setq debug-on-error t)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'ert)
(require 'init-git)
(require 'init-elpa)
(require 'init-utils)
(require 'init-platform)
(require 'init-appearance)
(require 'init-global-behaviour)
(require 'init-company)
(require 'init-prog)
(require 'init-clojure)
(require 'init-cc)
(require 'init-js)
(require 'init-scala)
(require 'init-web)
(require 'init-go)
(require 'init-abbrev)
(require 'init-term)
(require 'init-mc)
(require 'init-markdown)
(require 'init-server)
(require 'init-eglot)
(require 'init-flycheck)
(require 'init-json)
(require 'init-cloudformation)
(require 'init-kbd-macros)
(require 'init-yasnippet)
(require 'init-codestyle)
(require 'init-haskell)
(require 'init-crypt)
(require 'init-copilot)
(require 'init-org)
(require 'init-global-bindings)
(require 'init-hydra)
(require 'init-projectile)
(require 'init-helm)
(require 'init-mu4e)
(require 'init-treemacs)
(require 'init-whatsapp)
(require 'init-presentations)
(require 'init-testing)
(provide 'init)
;;; init.el ends here
