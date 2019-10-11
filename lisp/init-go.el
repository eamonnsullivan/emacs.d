;;; -*- lexical-binding: t -*-
;;; init-go.el --- stuff related to coding in GO

(use-package go-mode)
(use-package go-eldoc
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(provide 'init-go)
