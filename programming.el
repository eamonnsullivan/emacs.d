;;; -*- lexical-binding: t -*-
;;; programming.el --- stuff related to coding

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

;; python
(use-package elpy
  :ensure t
  :defer t
  :after python
  :config
  (elpy-enable))
(add-hook 'python-mode-hook 'elpy-enable)

;; smartparens-mode
(use-package smartparens
  :ensure t
  :defer t
  :commands
  smartparens-strict-mode
  smartparens-mode
  sp-restrict-to-pairs-interactive
  sp-local-pair
  :diminish smartparens-mode
  :init
  (add-hook 'scala-mode-hook 'smartparens-mode)
  (add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
  (add-hook 'python-mode-hook 'smartparens-mode)
  (add-hook 'java-mode-hook 'smartparens-mode)
  (add-hook 'js2-mode-hook 'smartparens-mode)
  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)
  (sp-pair "(" ")" :wrap "C-(") ;; how do people live without this?
  (sp-pair "[" "]" :wrap "s-[") ;; C-[ sends ESC
  (sp-pair "{" "}" :wrap "C-{")
  ;; WORKAROUND https://github.com/Fuco1/smartparens/issues/543
  (bind-key "C-<left>" nil smartparens-mode-map)
  (bind-key "C-<right>" nil smartparens-mode-map)
  (bind-key "s-<delete>" 'sp-kill-sexp smartparens-mode-map)
  (bind-key "s-<backspace>" 'sp-backward-kill-sexp smartparens-mode-map))

;; show parens
(when-available 'show-paren-mode
                (show-paren-mode t))

;; documentation at point
(use-package eldoc
  :ensure t
  :defer t
  :diminish eldoc-mode
  :commands eldoc-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode))

;; company mode
(use-package company
  :ensure t
  :diminish company-mode)

;; cc-mode customizations.
(defun my-make-CR-do-indent ()
  (defvar c-mode-base-map)
  (define-key c-mode-base-map "\C-m" 'c-context-line-break))
(add-hook 'c-initialization-hook 'my-make-CR-do-indent)
(add-hook 'c++-initialization-hook 'my-make-CR-do-indent)

(add-hook 'c-mode-common-hook (lambda ()
                                (c-set-style "bsd")
                                (defvar c-basic-offset 4)
                                (setq indent-tabs-mode nil)
                                (font-lock-add-keywords nil
                                                        '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))
                                (auto-fill-mode 1)
                                (modify-syntax-entry ?_ "w")
                                (defvar c-font-lock-extra-types
                                      (list "gboolean"
                                            "gsize" "gssize"
                                            "gchar" "guchar"
                                            "gint" "gint8" "gint16" "gint32"
                                            "guint" "guint8" "guint16" "guint32"
                                            "gshort" "gushort" "glong" "gulong"
                                            "gfloat" "gdouble" "gpointer"
                                            "gconstpointer"
                                            "GList" "GSList" "GFunc" "GString"))))

;; javascript mode
(use-package js2-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  :config
  (add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2))))

(use-package editorconfig
  :ensure t
  :defer t
  :diminish editorconfig-mode
  :init
  (add-hook 'prog-mode-hook (editorconfig-mode 1))
  (add-hook 'text-mode-hook (editorconfig-mode 1)))

;; ensime
(use-package ensime
  :ensure t
  :pin melpa
  :config
  (setq ensime-use-helm t)
  (setq ensime-startup-notification nil)
  (setq ensime-startup-snapshot-notification nil))

(use-package sbt-mode
  :pin melpa
  :defer t)

(use-package scala-mode
  :pin melpa
  :defer t)

;; customize ensime's implementation/test goto configuration for the
;; BBC's slightly non-standard layout in some older projects. Going
;; from the implementation to the test file works OK, but going back
;; doesn't. I still need to fix this.
(defun bbc-goto-test--is-test-dir (dir)
  (let ((case-fold-search nil))
    (or
     (string-match-p "src/test/scala/$" dir)
     (string-match-p "../test/scala/$" dir)
     (string-match-p "../tests?/$" dir))))

(defconst bbc-test-template
  "package %TESTPACKAGE%

import org.scalatest.{FlatSpec, MustMatchers}

class %TESTCLASS% extends FlatSpec with MustMatchers {

  \"%IMPLCLASS%\" should \"have a test!\" in {
    fail(\"no test\")
  }
}
"
  "The value to insert into a new test file")

(defun bbc-impl-class-name (test-class)
  (let ((suffixes (ensime-get-goto-test-config :test-class-suffixes))
        (case-fold-search nil))
    (dolist (s suffixes)
      (when (string-match-p (concat s "$") test-class)
        (message (format "Found the class name: %s for test class %s"
                         (replace-regexp-in-string (concat s "$") "" test-class)
                         test-class))
        (return (replace-regexp-in-string (concat s "$") "" test-class))))))

(setq ensime-goto-test-config-defaults
      (list :test-class-names-fn #'ensime-goto-test--test-class-names
            :test-class-suffixes '("Spec" "Test" "Check" "Specification")
            :impl-class-name-fn #'bbc-impl-class-name
            :impl-to-test-dir-fn #'ensime-goto-test--impl-to-test-dir
            :is-test-dir-fn #'bbc-goto-test--is-test-dir
            :test-template-fn (lambda () bbc-test-template)))

;; projectile
(use-package projectile
  :demand
  :diminish projectile-mode
  :init   (setq projectile-use-git-grep t)
  :config (projectile-global-mode t)
  :bind   (("s-f" . projectile-find-file)
           ("s-F" . projectile-grep)))

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-switch-project-action 'helm-projectile-find-file)

;; feature-mode
(defvar feature-step-search-path "src/test/scala/steps/**/*Steps.scala")

;; Try to find the step defining the current feature
(defun select-current-line ()
  (interactive)
  (move-beginning-of-line nil)
  (set-mark-command nil)
  (move-end-of-line nil)
  (setq deactivate-mark nil))

(defun get-selected-text (beg end)
  "message region or \"empty string\" if none highlighted"
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (message "%s" (if (and beg end)
                    (buffer-substring-no-properties beg end)
                  "empty string")))

;; sql mode stuff
(when (require 'sql-upcase nil :noerror)
   (add-hook 'sql-mode-hook 'sql-upcase-mode)
   (add-hook 'sql-interactive-mode-hook 'sql-upcase-mode))

;; use web-mode for .jsx files
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

;; enable colour in compile and sbt modes (this doesn't work for cucumber)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'sbt-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

;;; programming.el ends here
