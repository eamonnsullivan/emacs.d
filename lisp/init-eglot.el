;;; -*- lexical-binding: t -*-
;;; init-eglot.el --- stuff related to the language server protocol


(use-package eglot
  :config
  (setq eglot-autoshutdown t)
  (add-to-list 'eglot-server-programs
               `((scala-mode scala-ts-mode)
                 . ,(alist-get 'scala-mode eglot-server-programs)))
  :hook ((prog-mode . eglot-ensure))
  :bind (("C-c C-l r" . eglot-rename)
         ("C-c C-l o" . eglot-code-action-organize-imports)
         ("C-c C-l q" . eglot-code-action-quickfix)
         ("C-c C-l e" . eglot-code-action-extract)
         ("C-c C-l i" . eglot-code-action-inline)
         ("C-c C-l f" . eglot-format)))

(provide 'init-eglot)
