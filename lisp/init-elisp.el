;;; init-elisp.el --- Some settings for when writing elisp -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan <me@eamonnsullivan.co.uk>

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Maintainer: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Created: 2026-03-26
;; Version: 0.1
;; Package-Requires: ((emacs "31.0"))
;; Keywords: programming, languages, tools
;; URL: https://github.com/eamonnsullivan/init-prog

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides initialisation and configuration for when
;; writing Emacs Lisp code.

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

(defun eds/switch-to-ielm ()
  "Switch to the `ielm' buffer, creating it if necessary."
  (interactive)
  (let ((ielm-buffer (get-buffer "*ielm*")))
    (if ielm-buffer
        (switch-to-buffer ielm-buffer)
      (ielm))))

(defun eds/elisp-defaults ()
  "Set up defaults for Emacs Lisp editing."
  (eldoc-mode +1)
  (rainbow-mode +1))

(add-hook 'emacs-lisp-mode-hook 'eds/elisp-defaults)
(add-hook 'ielm-mode-hook 'eldoc-mode)

(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'eds/switch-to-ielm)

(provide 'init-elisp)
;;; init-elisp.el ends here
