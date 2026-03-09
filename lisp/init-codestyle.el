;;; init-codestyle.el --- Code style and formatting initialisation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan <me@eamonnsullivan.co.uk>

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Maintainer: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Created: 2020-08-27
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: coding, style, formatting, tools
;; URL: https://github.com/eamonnsullivan/init-codestyle

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides initialisation and configuration for code style and formatting
;; in Emacs, supporting consistent coding standards and workflow.

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

(use-package editorconfig
  :diminish editorconfig-mode
  :config (editorconfig-mode 1))

(straight-use-package 'dtrt-indent)
(require 'dtrt-indent)
(setopt dtrt-indent-global-mode t)

(use-package smartparens
  :straight
  (smartparens :type git :host github :repo "Fuco1/smartparens")
  :commands
  smartparens-mode
  sp-restrict-to-pairs-interactive
  sp-local-pair
  :diminish smartparens-mode
  :hook ((scala-ts-mode . smartparens-mode)
         (python-ts-mode . smartparens-mode)
         (java-mode . smartparens-mode)
         (js2-mode . smartparens-mode)
         (markdown-mode . smartparens-mode)
         (html-mode . smartparens-mode)
         (clojure-mode . smartparens-mode)
         (emacs-lisp-mode . smartparens-mode))
  :config
  (progn
    (require 'smartparens-config)
    ;; pair management

    (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
    (sp-local-pair 'web-mode "<" nil :when '(my/sp-web-mode-is-code-context))
    (sp-local-pair 'scala-ts-mode "(" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair 'scala-ts-mode "{" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))

    (defun sp-restrict-c (sym)
      "Smartparens restriction on `SYM' for C-derived parenthesis."
      (sp-restrict-to-pairs-interactive "{([" sym))

    ;;; markdown-mode
    (sp-with-modes '(markdown-mode gfm-mode rst-mode)
      (sp-local-pair "*" "*" :bind "C-*")
      (sp-local-tag "2" "**" "**")
      (sp-local-tag "s" "```scheme" "```")
      (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

    ;;; html-mode
    (sp-with-modes '(html-mode sgml-mode web-mode)
      (sp-local-pair "<" ">"))

    (sp-use-smartparens-bindings)
    (sp-pair "(" ")" :wrap "C-(") ;; how do people live without this?
    (sp-pair "[" "]" :wrap "s-[") ;; C-[ sends ESC
    (sp-pair "{" "}" :wrap "C-{")
    (sp-pair "\"" "\"" :wrap "C-\"")

    ;; I use this for something else
    (unbind-key "M-<backspace>" smartparens-mode-map)))

(set-default 'indent-tabs-mode nil)

(provide 'init-codestyle)
;;; init-codestyle.el ends here
