;;; init-json.el --- JSON mode initialisation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan <me@eamonnsullivan.co.uk>

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Maintainer: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Created: 2018-05-10
;; Version: 0.1
;; Package-Requires: ((emacs "24.1"))
;; Keywords: json, languages, tools
;; URL: https://github.com/eamonnsullivan/init-json

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides initialisation and configuration for JSON mode,
;; supporting editing, syntax highlighting, and workflow enhancements for JSON files in Emacs.

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

(use-package json-mode
  :mode (("\\.json\\'" . json-ts-mode)
         ("\\.tmpl\\'" . json-ts-mode)
         ("\\.eslintrc\\'" . json-ts-mode))
  :config (setq-default js-indent-level 2)
  (add-hook 'json-mode-hook
            (lambda()
              (local-unset-key (kbd "C-c C-f"))
              (smartparens-mode t))))

(use-package json-reformat
  :after json-ts-mode
  :bind (("C-c r" . json-pretty-print)
         ("C-c C-f" . json-pretty-print-buffer)))

(provide 'init-json)
;;; init-json.el ends here
