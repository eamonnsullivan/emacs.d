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

(setq straight-repository-branch "develop")

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
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq straight-check-for-modifications '(watch-files find-when-checking))
(setq straight-find-executable "/usr/local/opt/findutils/libexec/gnubin/find")

(setq load-prefer-newer t) ;; load newest of byte-compiled/text

(defmacro when-available (func foo)
  "*Do something if FUNCTION is available."
  `(when (fboundp ,func) ,foo))

(prefer-coding-system 'utf-8-unix)
(set-language-environment "UTF-8")

(setq custom-file (make-temp-file "emacs-custom")) ;; Disable the auto-generated customize section.

;; The rest of my init file, broken up into libraries in the lisp directory
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-elpa)
(require 'init-org)
(require 'init-company)
(require 'init-utils)
(require 'init-proxy)
(require 'init-platform)
(require 'init-appearance)
(require 'init-global-bindings)
(require 'init-global-behaviour)
(require 'init-helm)
(require 'init-prog)
(require 'init-lisp)
(require 'init-python)
(require 'init-cc)
(require 'init-js)
(require 'init-scala)
(require 'init-web)
(require 'init-git)
(require 'init-java)
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
(provide 'init)
;;; init.el ends here
