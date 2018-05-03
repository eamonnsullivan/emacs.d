;;; -*- lexical-binding: t -*-
;;; init-prog.el --- stuff related to programming, in general

(defvar imenu-auto-rescan t)
;; general, all languages
(use-package editorconfig
  :ensure t
  :defer t
  :diminish editorconfig-mode
  :init
  (add-hook 'prog-mode-hook (editorconfig-mode 1))
  (add-hook 'text-mode-hook (editorconfig-mode 1)))

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
    :chords ((" j" . dumb-jump-go)
             (" k" . dumb-jump-back)
             (" h" . dumb-jump-quick-look)))

;; general refactoring menu
(use-package emr
  :config
  (add-hook 'prog-mode-hook 'emr-initialize)
  ;; Just hit M-RET to access your refactoring tools in any supported mode.
  (define-key prog-mode-map (kbd "M-RET") 'emr-show-refactor-menu))

;; enable colour in compile and sbt modes (this doesn't work for cucumber)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'sbt-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

(provide 'init-prog)
