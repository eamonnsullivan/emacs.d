;;; init-projectile.el --- Projectile mode initialisation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan <me@eamonnsullivan.co.uk>

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Maintainer: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Created: 2018-05-03
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: projectile, project, convenience, tools
;; URL: https://github.com/eamonnsullivan/init-projectile

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides initialisation and configuration for Projectile mode,
;; enabling project management and workflow enhancements in Emacs.

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

(use-package projectile
  :demand
  :diminish projectile-mode
  :init
  (setopt projectile-completion-system 'default
        projectile-switch-project-action 'projectile-find-file
        projectile-indexing-method 'alien)
  :config
  (projectile-mode t)
  (add-to-list 'projectile-globally-ignored-directories "node-modules")
  (add-to-list 'projectile-globally-ignored-files "node-modules")
  (add-to-list 'projectile-globally-ignored-files "*.semanticdb")
  (add-to-list 'projectile-globally-ignored-files "*.db")
  :bind   (("C-c p h" . projectile-find-file)
           ("C-c p p". projectile-switch-project)))

(provide 'init-projectile)
;;; init-projectile.el ends here
