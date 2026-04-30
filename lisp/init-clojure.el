;;; init-clojure.el --- Clojure mode initialisation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan <me@eamonnsullivan.co.uk>

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Maintainer: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Created: 2018-10-20
;; Version: 0.1
;; Package-Requires: ((emacs "31.0"))
;; Keywords: clojure, languages, tools
;; URL: https://github.com/eamonnsullivan/init-clojure

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides initialisation and configuration for Clojure mode
;; in Emacs, supporting enhanced editing, evaluation, and workflow.

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


(use-package
  eldoc
  :commands eldoc-mode
  :diminish eldoc-mode
  :hook ((emacs-lisp-mode . turn-on-eldoc-mode)))

(use-package parseedn)

;; (use-package package-lint)

(require 'init-flycheck)

(use-package
  cider
  :hook ((clojure-mode . turn-on-eldoc-mode)
         (clojure-mode . cider-mode))
  :config
  (require 'flycheck-clj-kondo)
  (dolist (checker '(clj-kondo-clj clj-kondo-cljs clj-kondo-cljc clj-kondo-edn))
    (setopt flycheck-checkers (cons checker (delq checker flycheck-checkers))))
  (require 'init-org)
  (setopt org-babel-clojure-backend 'cider
        cider-prompt-for-symbol nil
        cider-save-file-on-load t
        cider-font-lock-dynamically '(macro core function var)
        cider-repl-pop-to-buffer-on-connect 'display-only
        cider-overlays-use-font-lock t
        nrepl-hide-special-buffers t
        cider-repl-history-file ".cider-repl-history"
        cider-show-error-buffer t
        cider-auto-select-error-buffer t)
  (add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojurescript-mode)))

(use-package clojure-mode-extra-font-locking)

(use-package clj-refactor
  :straight
  (clj-refactor :type git :host github :repo "clojure-emacs/clj-refactor.el" :branch "master")
  :hook ((clojure-mode . clj-refactor-mode)
         (clojure-mode . yas-minor-mode))
  :config
  (cljr-add-keybindings-with-prefix "C-c r")
  (setopt cljr-warn-on-eval nil))

(use-package cider-hydra
  :hook ((cider-mode . cider-hydra-mode))
  :bind ("C-c e" . cider-hydra-eval/body))

(use-package kibit-helper)

(defun find-definition ()
  "Try to find definition of cursor via LSP otherwise fallback to cider."
  (interactive)
  (let ((cursor (point))
        (buffer (current-buffer)))
    (eglot-find-declaration)
    (when (and (eq buffer (current-buffer))
               (eq cursor (point)))
      (cider-find-var))))

(define-key clojure-mode-map (kbd "M-.") #'find-definition)
(define-key clojurec-mode-map (kbd "M-.") #'find-definition)
(define-key clojurescript-mode-map (kbd "M-.") #'find-definition)

(provide 'init-clojure)
;;; init-clojure.el ends here
