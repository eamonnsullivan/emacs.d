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

(defcustom eds-eca-libfprint-sandbox "eca-0d932e4e8925629feaf7f70c"
  "Docker sandbox name or ID for a standalone libfprint ECA session."
  :type 'string
  :group 'tools)

(defun eds-eca-sandbox-command (command roots)
  "Run a fresh ECA server via sbx for a lone local libfprint root.
COMMAND is the original server command, and ROOTS are its workspace folders.
The sandbox must mount the workspace at the same path as the host."
  (let ((root (car roots)))
    (if (and root
             (null (cdr roots))
             (not (file-remote-p root))
             (string-equal (file-name-nondirectory (directory-file-name root))
                           "libfprint"))
        ;; Keep stdin open, but do not allocate a TTY: ECA uses stdio JSON-RPC.
        (append (list "sbx" "--no-input" "exec" "--interactive"
                      "--workdir" (directory-file-name root) "--"
                      eds-eca-libfprint-sandbox "eca")
                (cdr command))
      command)))

(use-package eca
  :straight (:host github :repo "editor-code-assistant/eca-emacs")
  :commands (eca)
  :config
  (when (eq system-type 'gnu/linux)
    (setopt eca-custom-command '("/home/eamonn/bin/eca-bwrap" "server")
            eca-process-wrapper-function #'eds-eca-sandbox-command
            eca-send-process-id nil))
  :custom
  (eca-max-response-kb 2048)
  (when (not (eq system-type 'gnu/linux))
    (eca-provider 'gh-copilot)
    (eca-gh-copilot-server-command "copilot-language-server")))

(provide 'init-eca)
;;; init-eca.el ends here
