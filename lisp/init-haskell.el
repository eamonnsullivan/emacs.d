;;; init-haskell.el --- Haskell mode initialisation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan <me@eamonnsullivan.co.uk>

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Maintainer: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Created: 2022-12-29
;; Version: 0.1
;; Package-Requires: ((emacs "24.3"))
;; Keywords: haskell, languages, tools
;; URL: https://github.com/eamonnsullivan/init-haskell

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides initialisation and configuration for Haskell mode,
;; supporting editing, compilation, and workflow enhancements for Haskell programming in Emacs.

;;; Licence:

;; This programme is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public Licence as published by
;; the Free Software Foundation, either version 3 of the Licence, or
;; (at your option) any later version.

;; This programme is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public Licence for more details.

;; You should have received a copy of the GNU General Public Licence
;; along with this programme.  If not, see <https://www.gnu.org/licenses/>.

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
;;; init-haskell.el ends here
