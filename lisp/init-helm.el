;;; -*- lexical-binding: t -*-
;;; init-helm.el --- stuff related to helm-mode

(use-package helm
  :diminish helm-mode
  :config
  (require 'helm-config)
  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "M-x") 'undefined)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-h i") 'helm-info)
  (global-set-key (kbd "<f1>") 'helm-resume)
  (global-unset-key (kbd "C-x C-b"))
  (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "C-x C-d") 'helm-browse-project)
  (global-set-key (kbd "M-\"") 'helm-lsp-global-workspace-symbol)
  (define-key global-map (kbd "M-g a") 'helm-do-grep-ag)

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
  :bind (("\C-c r" . helm-do-grep-ag))
  :init
  (setq helm-ag-use-grep-ignore-list t)
  (setq helm-ag-use-agignore t)
  (setq helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
  (setq helm-ag-command-option "--all-text")
  (setq helm-ag-ignore-buffer-patterns '("\\.txt\\'" "\\.mkd\\'" "\\.class\\'"))
  (setq helm-ag-insert-at-point 'symbol))

(use-package helm-ls-git
  :bind (("C-c p h" . helm-browse-project)))

(provide 'init-helm)
