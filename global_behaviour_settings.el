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
    (setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
          backup-by-copying t    ; Don't delink hardlinks
          version-control t      ; Use version numbers on backups
          delete-old-versions t  ; Automatically delete excess backups
          kept-new-versions 20   ; how many of the newest versions to keep
          kept-old-versions 5    ; and how many of the old
          )
  (message "Directory does not exist: ~/.emacs.d/backups"))

;; use ibuffer instead of the older list-buffers
; (defalias 'list-buffers 'ibuffer)

;; delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package use-package-chords
    :ensure t
    :config
    (key-chord-mode 1))

(defun eds/switch-to-previous-buffer ()
    "Switch to previously open buffer.
  Repeated invocations toggle between the two most recently open buffers."
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) 1)))

(key-chord-define-global "JJ" 'eds/switch-to-previous-buffer)

;; undo-tree everywhere
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  :bind ("s-/" . undo-tree-visualize)
  :chords (("//" . undo-tree-visualize)))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
;;; global_behaviour_settings.el ends here
