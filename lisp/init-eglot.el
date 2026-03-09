;;; init-eglot.el --- Eglot mode initialisation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan <me@eamonnsullivan.co.uk>

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Maintainer: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Created: 2024-12-24
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: eglot, lsp, languages, tools
;; URL: https://github.com/eamonnsullivan/init-eglot

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides initialisation and configuration for Eglot,
;; enabling Language Server Protocol (LSP) support and enhanced coding workflow in Emacs.

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

(use-package eglot
  :config
  (setopt eglot-autoshutdown t
          eglot-extend-to-xref t)
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
;;; init-eglot.el ends here
