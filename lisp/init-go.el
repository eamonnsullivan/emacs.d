;;; -*- lexical-binding: t -*-
;;; init-go.el --- stuff related to coding in GO

(use-package go-mode
  :ensure t)
(use-package go-eldoc
  :ensure t
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(provide 'init-go)
