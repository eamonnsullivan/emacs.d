;;; -*- lexical-binding: t -*-
;;; init-mc.el --- setting some convenience keys for multiple-cursor mode

(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "<C-s-down>") 'mc/mark-next-like-this)
  (global-set-key (kbd "<C-s-up>") 'mc/unmark-next-like-this)
  (global-unset-key (kbd "M-<down-mouse-1>"))
  (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click))

(provide 'init-mc)
