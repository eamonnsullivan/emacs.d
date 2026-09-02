;;; init-lsp-mode.el --- lsp-mode initialisation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan <me@eamonnsullivan.co.uk>

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Maintainer: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Created: 2026-03-17
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: lsp, languages, tools
;; URL: https://github.com/eamonnsullivan/init-lsp-mode

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file configures lsp-mode as an alternative to Eglot and Eglotx.

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

;;; Code:

(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :hook ((prog-mode . lsp-deferred)
         (lsp-mode . (lambda ()
                       (add-hook 'before-save-hook #'lsp-format-buffer nil t))))
  :init
  (setq read-process-output-max (* 4 1024 1024))
  :config
  (setopt lsp-auto-guess-root t
          lsp-auto-execute-action nil
          lsp-completion-provider :none
          lsp-enable-file-watchers nil
          lsp-enable-folding nil
          lsp-enable-imenu t
          lsp-enable-indentation nil
          lsp-enable-on-type-formatting nil
          lsp-enable-snippet t
          lsp-enable-text-document-color nil
          lsp-enable-xref t
          lsp-headerline-breadcrumb-enable nil
          lsp-idle-delay 0.5
          lsp-keep-workspace-alive nil
          lsp-log-io nil
          lsp-modeline-code-actions-enable nil
          lsp-modeline-diagnostics-enable t
          lsp-warn-no-matched-clients nil
          lsp-restart 'auto-restart
          lsp-signature-auto-activate nil)
  :bind (("C-c C-l r" . lsp-rename)
         ("C-c C-l o" . lsp-organize-imports)
         ("C-c C-l q" . lsp-execute-code-action)
         ("C-c C-l f" . lsp-format-buffer)))

(use-package lsp-ui
  :straight t
  :after lsp-mode
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode))

(provide 'init-lsp-mode)
;;; init-lsp-mode.el ends here
