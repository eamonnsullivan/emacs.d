;;; -*- lexical-binding: t -*-
;;; init-helm.el --- stuff related to helm-mode

(require 'tramp)

(use-package all-the-icons)

;; describe key bindings for current mode, with searching
(use-package helm-descbinds
  :demand t
  :bind (("C-h b" . helm-descbinds))
  :config
  (message "loading helm-descbinds")
  (helm-descbinds-mode))


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

  (helm-mode 1))

(provide 'init-helm)
