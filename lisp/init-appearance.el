;;; -*- lexical-binding: t -*-
;;; init-appearance.el --- Code related to the look of emacs

(defun disable-all-themes ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

;; current fave
(use-package modus-themes
  :straight
  (modus-themes :type git :host gitlab :repo "protesilaos/modus-themes" :branch "main")
  :init
  (setq modus-themes-slanted-constructs t
        modus-themes-bold-constructs nil
        modus-themes-fringes nil ; {nil,'subtle,'intense}
        modus-themes-mode-line '3d ; {nil,'3d,'moody}
        modus-themes-syntax nil ; Lots of options---continue reading the manual
        modus-themes-intense-hl-line nil
        modus-themes-paren-match 'subtle-bold ; {nil,'subtle-bold,'intense,'intense-bold}
        modus-themes-links 'neutral-underline ; Lots of options---continue reading the manual
        modus-themes-no-mixed-fonts nil
        modus-themes-prompts nil ; {nil,'subtle,'intense}
        modus-themes-completions nil ; {nil,'moderate,'opinionated}
        modus-themes-region 'bg-only-no-extend ; {nil,'no-extend,'bg-only,'bg-only-no-extend}
        modus-themes-diffs nil ; {nil,'desaturated,'fg-only,'bg-only}
        modus-themes-org-blocks nil ; {nil,'grayscale,'rainbow}
        modus-themes-headings ; Lots of options---continue reading the manual
        '((1 . section)
          (2 . section-no-bold)
          (3 . rainbow-line)
          (t . rainbow-line-no-bold))
        modus-themes-variable-pitch-headings nil
        modus-themes-scale-headings nil
        modus-themes-scale-1 1.1
        modus-themes-scale-2 1.15
        modus-themes-scale-3 1.21
        modus-themes-scale-4 1.27
        modus-themes-scale-5 1.33)
  :config
  (modus-themes-load-operandi)
  :demand t)

(defun modus-themes-toggle ()
  (if (eq (car custom-enabled-themes) 'modus-operandi)
      (modus-themes-load-vivendi)
    (modus-themes-load-operandi)))

;; Light at sunrise
(run-at-time (nth 1 (split-string (sunrise-sunset)))
             (* 60 60 24)
             (lambda ()
               (modus-themes-load-operandi)))

;; Dark at sunset
(run-at-time (nth 4 (split-string (sunrise-sunset)))
             (* 60 60 24)
             (lambda ()
               (modus-themes-load-vivendi)))

;; others I've tried
(use-package poet-theme
  :straight
  (poet-theme :type git :host github :repo "kunalb/poet")
  :demand t)

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
    (when (and (window-system) (< emacs-major-version 27))
      (message "toggling tool bar off")
      (tool-bar-mode -1))
    (when (window-system)
      (message "toggling scroll bar off")
      (toggle-scroll-bar -1))
    (line-number-mode t)
    (display-time-mode t)
    (setq display-time-24hr-format t)
    (when (and (window-system) (< emacs-major-version 27))
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
