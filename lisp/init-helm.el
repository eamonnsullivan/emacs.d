;;; -*- lexical-binding: t -*-
;;; init-helm.el --- stuff related to helm-mode

(require 'tramp)

(use-package all-the-icons)

(use-package helm-ls-git
  :demand t
  :config
  (message "loading helm-ls-git")
  :bind (("C-c p h" . helm-browse-project)))

(use-package helm
  :diminish helm-mode
  :config
  (message "loading helm")
  ;; (require 'helm-config)
  (define-key global-map [remap find-file] 'helm-find-files)
  (define-key global-map [remap occur] 'helm-occur)
  (define-key global-map [remap list-buffers] 'helm-buffers-list)
  (define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
  (define-key global-map [remap execute-extended-command] 'helm-M-x)
  (define-key global-map [remap apropos-command] 'helm-apropos)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-x b") 'helm-mini)
  (unless (boundp 'completion-in-region-function)
    (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
    (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action

  (helm-mode 1))

(use-package helm-ag
  :init
  (setopt helm-ag-use-grep-ignore-list t
        helm-ag-use-agignore t
        helm-ag-base-command "ag --nocolor --nogroup --ignore-case"
        helm-ag-command-option "--all-text"
        helm-ag-ignore-buffer-patterns '("\\.txt\\'" "\\.mkd\\'" "\\.class\\'")
        helm-ag-insert-at-point 'symbol))

(provide 'init-helm)
