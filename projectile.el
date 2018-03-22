;;; -*- lexical-binding: t -*-
;;; projectile.el --- stuff related to projectile project management

;; Copyright (c) 2017 Eamonn Sullivan

;; Author: Eamonn Sullivan <eamonn.sullivan@gmail.com>
;; Maintainer: Eamonn Sullivan <eamonn.sullivan@gmail.com>
;; Created 23 March 2017

;; Homepage: https://github.com/eamonnsullivan/emacs.d

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;;; Code:

;; projectile
(use-package projectile
  :demand
  :diminish projectile-mode
  :init
  (setq projectile-use-git-grep t)
  (setq projectile-indexing-method 'native)
  :config
  (projectile-global-mode t)
  (helm-projectile-on)
  (setq projectile-completion-system 'helm)
  (setq projectile-switch-project-action 'helm-projectile-find-file)
  (add-to-list 'projectile-globally-ignored-directories "node-modules")
  (add-to-list 'projectile-globally-ignored-files "node-modules")
  :bind   (("s-f" . projectile-find-file)
           ("s-F" . projectile-grep)))






;;; projectile.el ends here
