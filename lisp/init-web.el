;;; init-web.el --- Web development initialisation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan <me@eamonnsullivan.co.uk>

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Maintainer: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Created: 2018-05-03
;; Version: 0.1
;; Package-Requires: ((emacs "24.1"))
;; Keywords: web, html, css, tools, rainbow-mode
;; URL: https://github.com/eamonnsullivan/init-web

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides initialisation and configuration for web development,
;; including support for rainbow-mode and workflow enhancements in Emacs.

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

(defun my/web-mode-hook ()
  (setopt web-mode-enable-auto-pairing nil))

(defun my/sp-web-mode-is-code-context (id action context)
  (when (and (eq action 'insert)
             (not (or (get-text-property (point) 'part-side)
                      (get-text-property (point) 'block-side))))
    t))

(use-package web-mode
  :mode "\\.html?\\'"
  :config
  (progn
    (setopt web-mode-markup-indent-offset 2)
    (setopt web-mode-code-indent-offset 2)
    (setopt web-mode-enable-current-element-highlight t)
    (setopt web-mode-ac-sources-alist
          '(("css" . (ac-source-css-property))
            ("html" . (ac-source-words-in-buffer ac-source-abbrev)))
          )))

(provide 'init-web)
;;; init-web.el ends here
