;;; init-crypt.el --- Org crypt initialisation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan <me@eamonnsullivan.co.uk>

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Maintainer: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Created: 2024-04-01
;; Version: 0.1
;; Package-Requires: ((emacs "24.4"))
;; Keywords: org, crypt, encryption, privacy
;; URL: https://github.com/eamonnsullivan/init-crypt

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides initialisation and configuration for org-crypt,
;; enabling encryption and privacy features for Org mode documents.

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

(require 'org-crypt)
(require 'epa-file)
(epa-file-enable)
(org-crypt-use-before-save-magic)
(setopt org-tags-exclude-from-inheritance (quote ("crypt")))
(setopt org-crypt-key "499CD6EA565D20EA6296719F2A3D60BDFE015FFD")

(provide 'init-crypt)
;;; init-crypt.el ends here
