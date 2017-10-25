;;; -*- lexical-binding: t -*-
;;; global_behavior_settings.el --- Things I always want, no matter the mode

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

;; never use tabs
(setq-default indent-tabs-mode nil)

;; Turn off the annoying default backup behaviour
(if (file-directory-p "~/.emacs.d/backups")
    (setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
  (message "Directory does not exist: ~/.emacs.d/backups"))

;; use ibuffer instead of the older list-buffers
; (defalias 'list-buffers 'ibuffer)

;; delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; undo-tree everywhere
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

;;; global_behaviour_settings.el ends here
