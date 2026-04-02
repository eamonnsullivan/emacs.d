;;; init-testing.el --- Testing framework initialisation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan <me@eamonnsullivan.co.uk>

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Maintainer: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Created: 2025-12-05
;; Version: 0.1
;; Package-Requires: ((emacs "24.3"))
;; Keywords: testing, buttercup, tools
;; URL: https://github.com/eamonnsullivan/init-testing

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides initialisation and configuration for Buttercup,
;; enabling unit testing and workflow enhancements in Emacs.

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

(use-package buttercup)
(use-package undercover
  :config
  (undercover "lisp/eds-*.el" "tests/*.el"))

(use-package coverage)

(use-package el-mock)

(provide 'init-testing)
;;; init-testing.el ends here
