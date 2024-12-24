;;; -*- lexical-binding: t -*-
;;; init-eglot.el --- stuff related to the language server protocol

(use-package eglot
  :hook ((prog-mode . eglot-ensure)))

(provide 'init-eglot)
