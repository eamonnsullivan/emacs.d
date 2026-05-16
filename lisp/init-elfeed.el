;;; init-elfeed.el --- Elfeed initialisation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan <me@eamonnsullivan.co.uk>

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Maintainer: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Created: 2026-05-16
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: blogs, feed, web
;; URL: https://github.com/eamonnsullivan/init-mu4e

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides initialisation and configuration for mu4e,
;; enabling advanced email management and workflow enhancements in Emacs.

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

(use-package elfeed)

(use-package elfeed-goodies)

(require 'eds-org)

(use-package elfeed-org
:custom
(rmh-elfeed-org-files (list (concat (eds-org/get-org-directory) "/20251231133151-blogs.org")))
:config
(elfeed-org))

(provide 'init-elfeed)
;;; init-elfeed.el ends here
