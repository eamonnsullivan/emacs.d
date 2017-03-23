;;; -*- lexical-binding: t -*-
;;; org.el --- org mode stuff

;; Copyright (c) 2017 Eamonn Sullivan

;; Author: Eamonn Sullivan <eamonn.sullivan@gmail.com>
;; Maintainer: Eamonn Sullivan <eamonn.sullivan@gmail.com>
;; Created 23 March 2017

;; Homepage: https://github.com/eamonnsullivan/emacs.d

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;;; Code:

(defvar org-directory)
(defvar org-default-notes-file)
(defvar org-capture-templates)

(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline (concat org-directory "/tasks.org") "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("i" "Ideas" entry (file+headline (concat org-directory "/ideas.org") "Ideas"))
        ("n" "Notes" entry (file+headline (concat org-directory "/notes.org") "General Notes"))))

;; mobile org
(defvar org-mobile-directory)
(defvar org-mobile-inbox-for-pull)
(defvar org-mobile-files)
(require 'org-mobile)
(setq org-mobile-files (list (concat org-directory "/notes.org")
                             (concat org-directory "/tasks.org")
                             (concat org-directory "/ideas.org")
                             (concat org-directory "/recipes.org")
                             (concat org-directory "/svp.org")
                             (concat org-directory "/weight_history.org")))
(setq org-mobile-inbox-for-pull (concat org-directory "/inbox.org"))
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

;;; org.el ends here
