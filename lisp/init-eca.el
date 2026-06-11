;;; init-eca.el --- ECA initialisation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan <me@eamonnsullivan.co.uk>

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Maintainer: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Created: 2026-06-05
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: copilot, completion, tools, convenience
;; URL: https://github.com/eamonnsullivan/emacs.d

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This file configures ECA (Editor Code Assistant) to talk to the
;; GitHub Copilot API via the official Copilot language server.
;;
;; Prereqs (outside Emacs):
;;   npm i -g @github/copilot-language-server
;;
;; Then in Emacs:
;;   M-x eca
;;

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

(use-package eca
  :straight (:host github :repo "editor-code-assistant/eca-emacs")
  :commands (eca)
  :hook
  (prog-mode . eca-completion-mode)
  :bind (("C-c C-c" . eca-completion-accept))
  :custom
  (eca-max-response-kb 2048)
  (eca-provider 'gh-copilot)
  (eca-gh-copilot-server-command "copilot-language-server"))

(provide 'init-eca)
;;; init-eca.el ends here
