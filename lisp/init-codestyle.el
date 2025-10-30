;;; -*- lexical-binding: t -*-
;;; init-codestyle.el -- Bits related to formatting code.

(use-package editorconfig
  :diminish editorconfig-mode
  :config (editorconfig-mode 1))

(straight-use-package 'dtrt-indent)
(require 'dtrt-indent)
(setq dtrt-indent-global-mode t)

(use-package smartparens
  :straight
  (smartparens :type git :host github :repo "Fuco1/smartparens")
  :commands
  smartparens-mode
  sp-restrict-to-pairs-interactive
  sp-local-pair
  :diminish smartparens-mode
  :hook ((scala-mode . smartparens-mode)
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
    (sp-local-pair 'scala-mode "(" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair 'scala-mode "{" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))

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
