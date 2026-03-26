;;; init-company.el --- Company mode initialisation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan <me@eamonnsullivan.co.uk>

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Maintainer: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Created: 2018-05-02
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: completion, convenience, tools
;; URL: https://github.com/eamonnsullivan/init-company

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides initialisation and configuration for Company mode,
;; enabling in-buffer completion and improved editing workflow.

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

(use-package company
  :demand t
  :diminish company-mode
  :commands company-mode
  :config
  (global-company-mode)
  (setopt company-global-modes '(not term-mode)
          company-minimum-prefix-length 2
          company-selection-wrap-around t
          company-show-numbers t
          company-tooltip-align-annotations t
          company-idle-delay 0.5
          company-tooltip-limit 10
          company-tooltip-flip-when-above t)
  (add-to-list 'company-backends 'company-capf))

(use-package company-box
  :hook (company-mode . company-box-mode))

(provide 'init-company)
;;; init-company.el ends here
