;;; -*- lexical-binding: t -*-
;;; init-helm.el --- stuff related to helm-mode

(require 'tramp)

(use-package all-the-icons)

(use-package helm
  :diminish helm-mode
  :config
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
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

  (setq helm-move-to-line-cycle-in-source     t
        helm-ff-search-library-in-sexp        t
        helm-scroll-amount                    8
        helm-ff-file-name-history-use-recentf t
        helm-echo-input-in-header-line        t
        helm-split-window-inside-p            t
        helm-idle-delay                     0.0
        helm-input-idle-delay              0.01
        helm-yas-display-key-on-candidate     t)

  (defun helm-hide-minibuffer-maybe ()
    "Hide minibuffer in Helm session if we use the header line as input field."
    (when (with-helm-buffer helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'face
                     (let ((bg-color (face-background 'default nil)))
                       `(:background ,bg-color :foreground ,bg-color)))
        (setq-local cursor-type nil))))

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))


  (add-hook 'helm-minibuffer-set-up-hook
            'helm-hide-minibuffer-maybe)

  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 40)
  (helm-autoresize-mode 1)

  (helm-mode 1))

;; describe key bindings for current mode, with searching
(use-package helm-descbinds
  :bind (("C-h b" . helm-descbinds))
  :config
  (helm-descbinds-mode))


;; helm-ag stuff
(use-package helm-ag
  :init
  (setq helm-ag-use-grep-ignore-list t
        helm-ag-use-agignore t
        helm-ag-base-command "ag --nocolor --nogroup --ignore-case"
        helm-ag-command-option "--all-text"
        helm-ag-ignore-buffer-patterns '("\\.txt\\'" "\\.mkd\\'" "\\.class\\'")
        helm-ag-insert-at-point 'symbol))

(use-package helm-ls-git
  :bind (("C-c p h" . helm-browse-project)))

(provide 'init-helm)
