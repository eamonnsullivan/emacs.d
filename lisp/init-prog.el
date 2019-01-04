;;; -*- lexical-binding: t -*-
;;; init-prog.el --- stuff related to programming, in general

(defvar imenu-auto-rescan t)
;; general, all languages
(use-package editorconfig
  :diminish editorconfig-mode
  :init (add-hook 'after-init-hook #'editorconfig-mode))

;; smartparens-mode
(use-package smartparens
  :commands
  smartparens-mode
  sp-restrict-to-pairs-interactive
  sp-local-pair
  :diminish smartparens-mode
  :hook ((scala-mode . smartparens-mode)
         (python-mode . smartparens-mode)
         (java-mode . smartparens-mode)
         (js2-mode . smartparens-mode)
         (markdown-mode . smartparens-mode)
         (html-mode . smartparens-mode)
         (clojure-mode . smartparens-mode))
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

    (bind-key "s-<delete>" (sp-restrict-c 'sp-kill-sexp) scala-mode-map)
    (bind-key "s-<backspace>" (sp-restrict-c 'sp-backward-kill-sexp) scala-mode-map)
    (bind-key "s-<home>" (sp-restrict-c 'sp-beginning-of-sexp) scala-mode-map)
    (bind-key "s-<end>" (sp-restrict-c 'sp-end-of-sexp) scala-mode-map)

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

    ;; I use this for something else
    (unbind-key "M-<backspace>" smartparens-mode-map)))

;; show parens
(when-available 'show-paren-mode
                (show-paren-mode t))

;; comint
(require 'comint)
(setq ansi-color-for-comint-mode t)
(defun eds/init-comint ()
  ;; Don't jump around when output in a buffer happens
  (set (make-local-variable 'scroll-conservatively) 1000))
(add-hook 'comint-mode-hook 'eds/init-comint)


;; dumb-jump
(use-package dumb-jump
  :init
  (add-hook 'prog-mode-hook 'dumb-jump-mode)
  :diminish dumb-jump-mode
  :chords ((" j" . dumb-jump-go)
           (" k" . dumb-jump-back)
           (" h" . dumb-jump-quick-look)))

;; general refactoring menu
(use-package emr
  :config
  (add-hook 'prog-mode-hook 'emr-initialize)
  ;; Just hit M-RET to access your refactoring tools in any supported mode.
  (define-key prog-mode-map (kbd "M-RET") 'emr-show-refactor-menu))

(use-package highlight-symbol
  :bind (("M-n" . highlight-symbol-next)
         ("M-p" . highlight-symbol-prev))
  :chords (("SS" . highlight-symbol)))

(use-package groovy-mode)

;; enable colour in compile and sbt modes (this doesn't work for cucumber)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'sbt-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

;; Highlight Comment Annotations
;; from: https://gitlab.com/psachin/emacs.d/blob/master/custom_functions.org
(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.
This functions should be added to the hooks of major modes for
programming."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))
(add-hook 'prog-mode-hook 'font-lock-comment-annotations)

(use-package floobits)

(provide 'init-prog)
