;;; -*- lexical-binding: t -*-
;;; init-haskell.el --- stuff related to haskell

(use-package haskell-mode
  :preface
  (defun slot/haskell-load-and-bring ()
    "Sane behaviour when loading the current file into ghci."
    (interactive)
    (save-buffer)
    (haskell-process-load-file)
    (haskell-interactive-bring))

  :bind (:map haskell-mode-map
              ("C-c M-." . hoogle                         )
              ;; Jump to the import blocks and back in current file.
              ([f12]     . haskell-navigate-imports       )
              ([f11]     . haskell-navigate-imports-return)
              ;; Interactive stuff
              ("C-c C-c" . slot/haskell-load-and-bring    )
              ("C-c C-z" . haskell-interactive-switch     )
              ("C-M-;"   . haskell-mode-jump-to-def-or-tag))
  :custom
  (haskell-interactive-popup-errors nil) ; Don't pop up errors in a separate buffer.
  (haskell-process-type 'stack-ghci)
  (haskell-process-path-ghci "stack")
  (haskell-indentation-where-pre-offset  1)
  (haskell-indentation-where-post-offset 1)
  (haskell-process-auto-import-loaded-modules t))

(provide 'init-haskell)
