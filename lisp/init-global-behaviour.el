;;; -*- lexical-binding: t -*-
;;; init-global-behavior.el --- Things I always want, no matter the mode

;; never use tabs
(setq-default indent-tabs-mode nil)
(setq delete-by-moving-to-trash t) ;; move to trash folder
(setq require-final-newline t) ;; unix world works better with a final newline.
;; Turn off the annoying default backup behaviour
(if (file-directory-p "~/.emacs.d/backups")
    (setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
          backup-by-copying t    ; Don't delink hardlinks
          version-control t      ; Use version numbers on backups
          delete-old-versions t  ; Automatically delete excess backups
          kept-new-versions 20   ; how many of the newest versions to keep
          kept-old-versions 5    ; and how many of the old
          )
  (message "Directory does not exist: ~/.emacs.d/backups"))

;; delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq-default major-mode 'text-mode)
(add-hook 'text-mode-hook
          (lambda ()
            (turn-on-auto-fill)
            (diminish 'auto-fill-function)))
(setq sentence-end-double-space nil)

(use-package use-package-chords
  :ensure t
  :init
  (setq key-chord-two-keys-delay 0.05)
  :config
  (key-chord-mode 1))

(defun eds/switch-to-previous-buffer ()
    "Switch to previously open buffer.
  Repeated invocations toggle between the two most recently open buffers."
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) 1)))

(key-chord-define-global "JJ" 'eds/switch-to-previous-buffer)

;; undo-tree everywhere
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  :bind ("s-/" . undo-tree-visualize)
  :chords (("zz" . undo-tree-visualize)))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(use-package hippie-exp                 ; Powerful expansion and completion
  :bind (([remap dabbrev-expand] . hippie-expand))
  :config
  (progn
    (setq hippie-expand-try-functions-list
          '(try-expand-dabbrev
            try-expand-dabbrev-all-buffers
            try-expand-dabbrev-from-kill
            try-complete-file-name-partially
            try-complete-file-name
            try-expand-all-abbrevs
            try-expand-list
            try-complete-lisp-symbol-partially
            try-complete-lisp-symbol))))

(use-package autorevert ; Auto-revert buffers of changed files
  :init (global-auto-revert-mode)
  :config
  (setq auto-revert-verbose nil
        global-auto-revert-non-file-buffers t)

  (when (eq system-type 'darwin)
    ;; File notifications aren't supported on OS X
    (setq auto-revert-use-notify nil))
  :diminish (auto-revert-mode . " Ⓐ"))

(use-package swiper
  :bind ("C-S-s" . swiper))

;; Jump to things in Emacs tree-style
(use-package avy
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0))
  :init (add-hook 'after-init-hook #'avy-setup-default)
  :config (setq avy-background t))

;; Display available keybindings in popup
(use-package which-key
  :diminish which-key-mode
  :bind (:map help-map ("C-h" . which-key-C-h-dispatch))
  :init (add-hook 'after-init-hook #'which-key-mode))

(use-package discover-my-major
  :bind (("C-h M-m" . discover-my-major)
         ("C-h M-M" . discover-my-mode)))

(use-package crux
  :ensure t
  :bind (("C-k"                          . crux-smart-kill-line)
         ([(control shift return)]       . crux-smart-open-line-above)
         ([(shift return)]               . crux-smart-open-line)
         ([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ("s-r"                          . crux-recentf-find-file)
         ("C-c D"                        . crux-delete-file-and-buffer)
         ("C-c r"                        . crux-rename-file-and-buffer)
         ("C-c k"                        . crux-kill-other-buffers)))

;; font scaling
(use-package default-text-scale
  :ensure t)

(use-package dashboard
  :init
  (setq dashboard-items '((recents . 5)
                          (projects . 5)))
  :config
  (dashboard-setup-startup-hook))

(provide 'init-global-behaviour)
