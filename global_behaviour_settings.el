;; never use tabs
(setq-default indent-tabs-mode nil)

;; Turn off the annoying default backup behaviour
(if (file-directory-p "~/.emacs.d/backups")
    (setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
  (message "Directory does not exist: ~/.emacs.d/backups"))

;; use ibuffer instead of the older list-buffers
(defalias 'list-buffers 'ibuffer)

;; delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))
