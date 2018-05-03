;;; -*- lexical-binding: t -*-
;;; init-global-behavior.el --- Things I always want, no matter the mode

;; never use tabs
(setq-default indent-tabs-mode nil)

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

;; use ibuffer instead of the older list-buffers
; (defalias 'list-buffers 'ibuffer)

;; delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package use-package-chords
    :ensure t
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
  :ensure t
  :bind ("C-S-s" . swiper))

(provide 'init-global-behaviour)
