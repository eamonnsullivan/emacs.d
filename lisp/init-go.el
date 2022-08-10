;;; -*- lexical-binding: t -*-
;;; init-go.el --- stuff related to coding in GO

(use-package go-mode)

(require 'init-lsp)

(lsp-register-custom-settings
 '(("gopls.completeUnimported" t t)
   ("gopls.staticcheck" t t)
   ("gopls.allExperiments" t t)))

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(provide 'init-go)
