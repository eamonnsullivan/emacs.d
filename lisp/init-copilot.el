;;; init-copilot.el --- Copilot mode initialisation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan <me@eamonnsullivan.co.uk>

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Maintainer: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Created: 2024-10-29
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: copilot, completion, tools, convenience
;; URL: https://github.com/eamonnsullivan/init-copilot

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides initialisation and configuration for Copilot mode,
;; enabling AI-powered code completion and workflow enhancements in Emacs.

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


(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t
  :hook
  (prog-mode . copilot-mode)
  (prog-mode . copilot-nes-mode)
  :bind (:map copilot-completion-map
              ("C-c C-c" . copilot-accept-completion)
              ("C-c C-c" . copilot-accept-completion)
              ("C-n" . copilot-next-completion)
              ("C-p" . copilot-previous-completion))
  :config
  ;; Remap for copilot, since I don't actually use this key in python
  ;; much.
  (unbind-key "C-c C-c" python-ts-mode-map)
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-to-list 'copilot-indentation-alist '(closure-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2)))

(use-package copilot-chat
  :straight (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
  :config
  (global-unset-key (kbd "C-c i"))
  :bind (("C-c i" . copilot-chat-goto-input))
  :after (request org markdown-mode))

;; (add-hook 'git-commit-setup-hook 'copilot-chat-insert-commit-message)


(provide 'init-copilot)
;;; init-copilot.el ends here
