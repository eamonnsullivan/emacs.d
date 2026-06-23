;;; init-term.el --- Terminal initialisation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan <me@eamonnsullivan.co.uk>

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Maintainer: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Created: 2017-01-02
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: terminal, vterm, tools, convenience
;; URL: https://github.com/eamonnsullivan/init-term

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides initialisation and configuration for terminal support in Emacs,
;; including integration with vterm for enhanced terminal emulation and workflow.

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

(require 'eds-utils)

(use-package ghostel
  :straight (:host github :repo "dakra/ghostel")
  :bind (("C-c t n" . eds-utils/visit-term)
         ("C-c t s" . eds-utils/ssh-term)
         :map ghostel-semi-char-mode-map
         ("C-s"  . consult-line)
         ("M-<backspace>" . ghostel-backward-kill-word))
  :config
  (setopt ghostel-tramp-shell-integration t
          ghostel-module-auto-install 'download)
  (defun ghostel-send-C-k-and-kill ()
    "Send `C-k' to ghostel.
Like normal Emacs `C-k'.  Kill to end of line and put content in kill-ring."
    (interactive)
    (kill-ring-save (point) (line-end-position))
    (ghostel-send-key "k" "ctrl")))

(provide 'init-term)
;;; init-term.el ends here
