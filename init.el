;;; init.el --- Initialization code for emacs

;; Copyright (c) 2018 Eamonn Sullivan

;; Author: Eamonn Sullivan <eamonn.sullivan@gmail.com>
;; Maintainer: Eamonn Sullivan <eamonn.sullivan@gmail.com>
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

;; (defvar comp-deferred-compilation-deny-list ())

(setq straight-repository-branch "develop"
      straight-use-package-by-default t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (unless (catch 'emacs-version-changed
	    (load bootstrap-file nil 'nomessage))
    (when (boundp 'comp-eln-load-path)
      ;; remove leftovers, with confirmation just to be safe
      (when (yes-or-no-p (format "Delete '%s'?" (car comp-eln-load-path)))
	(delete-directory (expand-file-name (car comp-eln-load-path)) t))
      ;; and try again
      (load bootstrap-file nil 'nomessage))))

(straight-use-package 'use-package)

(cond
 ((eq system-type 'gnu/linux)
  (setq straight-find-executable "/usr/bin/find"))
 ((eq system-type 'darwin)
  (setq straight-find-executable "/usr/local/opt/findutils/libexec/gnubin/find")))

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

;; The rest of my init file, broken up into libraries in the lisp directory
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-elpa)
(require 'init-helm)
(require 'init-org)
(require 'init-utils)
(require 'init-platform)
(require 'init-appearance)
(require 'init-global-bindings)
(require 'init-global-behaviour)
(require 'init-company)
(require 'init-prog)
(require 'init-clojure)
(require 'init-python)
(require 'init-cc)
(require 'init-js)
(require 'init-scala)
(require 'init-web)
(require 'init-git)
(require 'init-go)
(require 'init-abbrev)
(require 'init-term)
(require 'init-flycheck)
(require 'init-mc)
(require 'init-projectile)
(require 'init-markdown)
(require 'init-server)
(require 'init-lsp)
(require 'init-hydra)
(require 'init-ts)
(require 'init-json)
(require 'init-kbd-macros)
(require 'init-slack)
(require 'init-yasnippet)
(require 'init-codestyle)
(provide 'init)
;;; init.el ends here
