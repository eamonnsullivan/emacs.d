;;; -*- lexical-binding: t -*-
;;; init-appearance.el --- Code related to the look of emacs

(defun disable-all-themes ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

;; current fave
(use-package poet-theme
  :straight
  (poet-theme :type git :host github :repo "kunalb/poet")
  :demand t
  :config
  (load-theme 'poet t))

;; others I've tried
(use-package leuven-theme
  :demand t
  :straight
  (leuven-theme :type git :host github :repo "fniessen/emacs-leuven-theme"))

(use-package zenburn-theme
  :demand t)
(use-package vscode-dark-plus-theme
  :straight
  (vscode-dark-plus-theme :type git :host github :repo "ianpan870102/vscode-dark-plus-emacs-theme")
  :demand t)
(use-package material-theme
  :demand t)
(use-package parchment-theme
  :demand t)
(use-package doom-themes
  :demand t)
(use-package modus-vivendi-theme
  :demand t
  :config
  (setq modus-vivendi-theme-slanted-constructs nil)
  (setq modus-vivendi-theme-bold-constructs nil)
  (setq modus-vivendi-theme-proportional-fonts nil)
  (setq modus-vivendi-theme-scale-headings nil)
  (setq modus-vivendi-theme-scale-1 1.05)
  (setq modus-vivendi-theme-scale-2 1.1)
  (setq modus-vivendi-theme-scale-3 1.15)
  (setq modus-vivendi-theme-scale-4 1.2))
(use-package modus-operandi-theme
  :demand t
  :config
  (setq modus-operandi-theme-slanted-constructs nil)
  (setq modus-operandi-theme-bold-constructs nil)
  (setq modus-operandi-theme-proportional-fonts nil)
  (setq modus-operandi-theme-scale-headings nil)
  (setq modus-operandi-theme-scale-1 1.05)
  (setq modus-operandi-theme-scale-2 1.1)
  (setq modus-operandi-theme-scale-3 1.15)
  (setq modus-operandi-theme-scale-4 1.2))

(defun my-setup-main-fonts (default-height variable-pitch-height)
  "Set up default fonts.
Use DEFAULT-HEIGHT for default face and VARIABLE-PITCH-HEIGHT
for variable-pitch face."
  (message "Found Hack and Fira Sans, so setting up fonts.")
  (set-face-attribute 'default nil
                      :family "Hack"
                      :height default-height)
  (set-face-attribute 'variable-pitch nil
                      :family "Fira Sans"
                      :height variable-pitch-height
                      :weight 'regular))

(defun my-appearance-settings (&rest frame)
  "Apply my preferences on graphical appearance."
  (interactive "P")
  (progn
    (message "my-appearance-settings running.")
    (transient-mark-mode t)
    (menu-bar-mode -1)
    (when (window-system)
      (message "toggling tool bar off")
      (tool-bar-mode -1))
    (when (window-system)
      (message "toggling scroll bar off")
      (toggle-scroll-bar -1))
    (line-number-mode t)                      ; show line numbers
    (display-time-mode t)
    (setq display-time-24hr-format t)
    (when (window-system)
      (message "setting default-frame-alist")
      (setq default-frame-alist
            '((vertical-scroll-bars)
              (tool-bar-lines . 0)
              (menu-bar-lines . 0)
              (menu-bar-mode . -1)
              (tool-bar-mode . -1)
              (toggle-scroll-bar . -1))))

    ;; set some font options, if the fonts are available
    (if (and (find-font (font-spec :name "Hack"))
             (find-font (font-spec :name "Fira Sans")))
        (when window-system
          (if (> (x-display-pixel-width) 1800)
              (my-setup-main-fonts 130 140)
            (my-setup-main-fonts 110 120)))
      (set-face-attribute 'default nil :height 120))))


(require 'server)
(defadvice server-create-window-system-frame
  (after set-window-system-frame-colours ())
  "Set custom appearance settings when creating the first frame on a display"
  (message "Running after frame-initialize")
  (my-appearance-settings))
(ad-activate 'server-create-window-system-frame)
(add-hook 'after-make-frame-functions 'my-appearance-settings t)
(unless (version< emacs-version "27.0")
  (add-hook 'server-after-make-frame-hook 'my-appearance-settings t))

(provide 'init-appearance)
