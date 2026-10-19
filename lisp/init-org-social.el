;;; init-org.el --- Org mode configuration initialisation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan <me@eamonnsullivan.co.uk>

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Maintainer: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Created: 2026-10-18
;; Version: 0.1
;; Package-Requires: ((emacs "31") (org "9.8"))
;; Keywords: notes, organisation, productivity
;; URL: https://github.com/eamonnsullivan/init-org

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides initialisation and configuration for Org social.

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

(use-package request)
(use-package emojify)
(use-package org-social
  :custom
  (org-social-file "~/git/eamonnsullivan.co.uk/static/social.org")
  (org-social-relay "https://relay.org-social.org/")
  (org-social-my-public-url "https://eamonnsullivan.co.uk/social.org")
  :config
  (keymap-global-set "C-c s t" #'org-social-timeline)
  (keymap-global-set "C-c s n" #'org-social-new-post)
  (keymap-global-set "C-c s r" #'org-social-new-reply))

(provide 'init-org-social)

;;; init-org-social.el ends here
