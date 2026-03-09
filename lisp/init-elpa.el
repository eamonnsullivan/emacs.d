;;; init-elpa.el --- ELPA and straight.el initialisation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan <me@eamonnsullivan.co.uk>

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Maintainer: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Created: 2018-05-03
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: package, elpa, straight, tools
;; URL: https://github.com/eamonnsullivan/init-elpa

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides initialisation and configuration for ELPA and straight.el,
;; enabling robust package management and workflow enhancements in Emacs.

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

(straight-use-package 'use-package)
(use-package bind-key)
(use-package diminish)
; (setopt gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(provide 'init-elpa)
;;; init-elpa.el ends here
