;;; init-cc.el --- C/C++ mode initialisation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan <me@eamonnsullivan.co.uk>

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Maintainer: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Created: 2018-05-03
;; Version: 0.1
;; Package-Requires: ((emacs "31.0"))
;; Keywords: c, c++, languages, tools
;; URL: https://github.com/eamonnsullivan/init-cc

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides initialisation and configuration for C and C++ modes
;; in Emacs, supporting enhanced editing, compilation, and workflow.

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

(defun my-make-CR-do-indent ()
  (defvar c-mode-base-map)
  (define-key c-mode-base-map "\C-m" 'c-context-line-break))
(add-hook 'c-initialization-hook 'my-make-CR-do-indent)
(add-hook 'c++-initialization-hook 'my-make-CR-do-indent)

(add-hook 'c-mode-common-hook (lambda ()
                                (c-set-style "bsd")
                                (defvar c-basic-offset 4)
                                (setopt indent-tabs-mode nil)
                                (font-lock-add-keywords nil
                                                        '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))
                                (auto-fill-mode 1)
                                (modify-syntax-entry ?_ "w")
                                (defvar c-font-lock-extra-types
                                      (list "gboolean"
                                            "gsize" "gssize"
                                            "gchar" "guchar"
                                            "gint" "gint8" "gint16" "gint32"
                                            "guint" "guint8" "guint16" "guint32"
                                            "gshort" "gushort" "glong" "gulong"
                                            "gfloat" "gdouble" "gpointer"
                                            "gconstpointer"
                                            "GList" "GSList" "GFunc" "GString"))))

(use-package zig-mode)

(provide 'init-cc)
;;; init-cc.el ends here
