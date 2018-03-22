;;; -*- lexical-binding: t -*-
;;; programming.el --- stuff related to coding

;; Copyright (c) 2017 Eamonn Sullivan

;; Author: Eamonn Sullivan <eamonn.sullivan@gmail.com>
;; Maintainer: Eamonn Sullivan <eamonn.sullivan@gmail.com>
;; Created 23 March 2017

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

(defvar imenu-auto-rescan t)
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
  (add-hook 'emacs-lisp-mode-hook 'show-smartparens-mode)
  (add-hook 'python-mode-hook 'smartparens-mode)
  (add-hook 'java-mode-hook 'smartparens-mode)
  (add-hook 'js2-mode-hook 'smartparens-mode)
  :config
    (progn
      (require 'smartparens-config)
      ;; pair management

      (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
      (sp-local-pair 'web-mode "<" nil :when '(my/sp-web-mode-is-code-context))

      ;;; markdown-mode
      (sp-with-modes '(markdown-mode gfm-mode rst-mode)
        (sp-local-pair "*" "*" :bind "C-*")
        (sp-local-tag "2" "**" "**")
        (sp-local-tag "s" "```scheme" "```")
        (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

      ;;; html-mode
      (sp-with-modes '(html-mode sgml-mode web-mode)
        (sp-local-pair "<" ">"))

       ;;; lisp modes
      (sp-with-modes sp--lisp-modes
        (sp-local-pair "(" nil :bind "C-("))
      (sp-use-smartparens-bindings)
      (sp-pair "(" ")" :wrap "C-(") ;; how do people live without this?
      (sp-pair "[" "]" :wrap "s-[") ;; C-[ sends ESC
      (sp-pair "{" "}" :wrap "C-{")

    ;; WORKAROUND https://github.com/Fuco1/smartparens/issues/543
      (bind-key "C-<left>" nil smartparens-mode-map)
      (bind-key "C-<right>" nil smartparens-mode-map)
      (bind-key "s-<delete>" 'sp-kill-sexp smartparens-mode-map)
      (bind-key "s-<backspace>" 'sp-backward-kill-sexp smartparens-mode-map)))

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
  :diminish company-mode
  :commands company-mode
  :init
  (setq
   company-dabbrev-ignore-case nil
   company-dabbrev-code-ignore-case nil
   company-dabbrev-downcase nil
   company-idle-delay 0
   company-minimum-prefix-length 4)
  :config
  ;; disables TAB in company-mode, freeing it for yasnippet
  (define-key company-active-map [tab] nil)
  (define-key company-active-map (kbd "TAB") nil))

(use-package company-tern
  :ensure t)

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
(use-package nvm
  :commands (nvm-use nvm-use-for nvm--installed-versions))

(use-package rjsx-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.jsx$" . rjsx-mode))
  :config
  (add-hook 'rjsx-mode-hook
            (lambda()
              (setq indent-tabs-mode nil)
              (setq js-indent-level 2)
              (setq js2-strict-missing-semi-warning nil))))

(use-package js-comint
  :ensure t
  :config
  (add-hook 'js2-mode-hook
            (lambda ()
              (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
              (local-set-key (kbd "C-M-x") 'js-send-last-sexp-and-go)
              (local-set-key (kbd "C-c b") 'js-send-buffer)
              (local-set-key (kbd "C-c C-b") 'js-send-buffer-and-go)
              (local-set-key (kbd "C-c l") 'js-load-file-and-go))))

(use-package js2-refactor
  :ensure t)
(use-package js2-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  (add-to-list 'company-backends 'company-tern)
  :config
  (setq-default js2-ignored-warnings '("msg.extra.trailing.comma"))
  (add-hook 'js2-mode-hook (lambda ()
                             (setq js2-basic-offset 2)
                             (tern-mode)
                             (company-mode)
                             (js2-imenu-extras-mode)
                             (js2-refactor-mode)
                             (js2r-add-keybindings-with-prefix "C-c C-r"))))

(defun delete-tern-process()
  (interactive)
  (delete-process "Tern"))

(use-package editorconfig
  :ensure t
  :defer t
  :diminish editorconfig-mode
  :init
  (add-hook 'prog-mode-hook (editorconfig-mode 1))
  (add-hook 'text-mode-hook (editorconfig-mode 1)))

(use-package prettier-js
  :ensure t
  :diminish prettier-js-mode
  :config
  (setq prettier-js-args '(
                           "--trailing-comma" "es5"
                           "--single-quote" "true"
                           "--print-width" "100"))
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'rjsx-mode-hook 'prettier-js-mode))

;; ensime
(use-package ensime
  :ensure t
  :pin melpa-stable
  :config
  (setq ensime-search-interface 'helm)
  (setq ensime-use-helm t)
  (setq ensime-startup-notification nil)
  (setq ensime-startup-snapshot-notification nil)
  (add-hook 'scala-mode-hook 'ensime-mode))

(use-package sbt-mode
  :pin melpa-stable
  :defer t)

(use-package scala-mode
  :pin melpa-stable
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

;; feature-mode
(use-package feature-mode
  :ensure t
  :config
  (defvar feature-step-search-path "src/test/scala/steps/**/*Steps.scala"))

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
                 (List nil nil)))
  (message "%s" (if (and beg end)
                    (buffer-substring-no-properties beg end)
                  "empty string")))

;; sql mode stuff
(when (require 'sql-upcase nil :noerror)
   (add-hook 'sql-mode-hook 'sql-upcase-mode)
   (add-hook 'sql-interactive-mode-hook 'sql-upcase-mode))

(defun my/web-mode-hook ()
  (setq web-mode-enable-auto-pairing nil))

(defun my/sp-web-mode-is-code-context (id action context)
  (when (and (eq action 'insert)
             (not (or (get-text-property (point) 'part-side)
                      (get-text-property (point) 'block-side))))
    t))

(use-package web-mode
  :mode "\\.html?\\'"
  :config
  (progn
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-enable-current-element-highlight t)
    (setq web-mode-ac-sources-alist
          '(("css" . (ac-source-css-property))
            ("html" . (ac-source-words-in-buffer ac-source-abbrev)))
          )))

;; enable colour in compile and sbt modes (this doesn't work for cucumber)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'sbt-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

(use-package git-timemachine
  :ensure t
  :bind (("C-z g" . git-timemachine)))

(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode 't))

(use-package project-explorer
  :ensure t
  :bind (("C-z p" . project-explorer-open)))

;; java
(use-package eclim
  :ensure t
  :init
  (setq eclimd-autostart t)
  (add-hook 'java-mode-hook 'eclim-mode)
  (require 'eclimd)
  :config
  (setq help-at-pt-display-when-idle t)
  (setq help-at-pt-timer-delay 0.1)
  (help-at-pt-set-timer))

(add-hook 'java-mode-hook (lambda()
                            (setq c-basic-offset 4
                                  tab-width 4
                                  indent-tabs-mode nil)))

(use-package company-emacs-eclim
  :ensure t
  :init
  (company-emacs-eclim-setup)
  (global-company-mode t))

;; go
(use-package go-mode
  :ensure t)
(use-package go-eldoc
  :ensure t
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup))
(use-package company-go
  :ensure t
  :config
  (add-hook 'go-mode-hook (lambda ()
                            (set (make-local-variable 'company-backends) '(company-go))
                            (company-mode))))
;; comint
(require 'comint)
(setq ansi-color-for-comint-mode t)
(defun eds/init-comint ()
  ;; Don't jump around when output in a buffer happens
  (set (make-local-variable 'scroll-conservatively) 1000))
(add-hook 'comint-mode-hook 'eds/init-comint)


;; dumb-jump
  (use-package dumb-jump
    :ensure t
    :diminish dumb-jump-mode
    :chords ((".." . dumb-jump-go)
             (",," . dumb-jump-back)))

;; restclient
(use-package restclient
  :ensure t)

;;; programming.el ends here
