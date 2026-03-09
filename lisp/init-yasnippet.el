;;; init-yasnippet.el --- Yasnippet initialisation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan <me@eamonnsullivan.co.uk>

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Maintainer: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Created: 2019-06-05
;; Version: 0.1
;; Package-Requires: ((emacs "24.1"))
;; Keywords: yasnippet, snippets, convenience, tools
;; URL: https://github.com/eamonnsullivan/init-yasnippet

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides initialisation and configuration for yasnippet,
;; enabling snippet expansion and workflow enhancements in Emacs.

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

(use-package yasnippet
  :diminish (yas-minor-mode . " Ⓨ")
  :hook ((prog-mode) . yas-minor-mode)
  :config
  (use-package yasnippet-snippets :after yasnippet :demand t)
  (add-to-list 'yas-snippet-dirs "~/.config/emacs/snippets")
  (yas-global-mode 1)
  (with-eval-after-load 'company-mode
    (add-to-list 'company-backends '(company-yasnippet)))
  (yas-reload-all))

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
