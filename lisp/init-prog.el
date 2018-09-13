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
         (emacs-lisp-mode . smartparens-mode)
         (emacs-lisp-mode . show-smartparens-mode)
         (python-mode . smartparens-mode)
         (java-mode . smartparens-mode)
         (js2-mode . smartparens-mode)
         (markdown-mode . smartparens-mode)
         (html-mode . smartparens-mode))
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

      ;; I use this for something else
      (unbind-key "M-<backspace>" smartparens-mode-map)))

;; show parens
(when-available 'show-paren-mode
                (show-paren-mode t))

;; documentation at point
(use-package eldoc
  :commands eldoc-mode
  :diminish eldoc-mode
  :hook ((emacs-lisp-mode . turn-on-eldoc-mode)))

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

(provide 'init-prog)
