;;; -*- lexical-binding: t -*-
;;; init-appearance.el --- Code related to the look of emacs

(defun disable-all-themes ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

;; current fave
(use-package modus-vivendi-theme
  :straight
  (modus-themes :type git :host gitlab :repo "protesilaos/modus-themes")
  :demand t)

(use-package modus-operandi-theme
  :straight
  (modus-themes :type git :host gitlab :repo "protesilaos/modus-themes")
  :demand t)

(defmacro modus-themes-format-sexp (sexp &rest objects)
  `(eval (read (format ,(format "%S" sexp) ,@objects))))

(dolist (theme '("operandi" "vivendi"))
  (modus-themes-format-sexp
   (defun modus-%1$s-theme-load ()
     (setq modus-%1$s-theme-slanted-constructs nil
           modus-%1$s-theme-bold-constructs nil
           modus-%1$s-theme-fringes 'subtle ; {nil,'subtle,'intense}
           modus-%1$s-theme-mode-line '3d ; {nil,'3d,'moody}
           modus-%1$s-theme-faint-syntax t
           modus-%1$s-theme-intense-hl-line nil
           modus-%1$s-theme-intense-paren-match nil
           modus-%1$s-theme-prompts 'intense ; {nil,'subtle,'intense}
           modus-%1$s-theme-completions 'moderate ; {nil,'moderate,'opinionated}
           modus-%1$s-theme-diffs nil ; {nil,'desaturated,'fg-only}
           modus-%1$s-theme-org-blocks 'greyscale ; {nil,'greyscale,'rainbow}
           modus-%1$s-theme-variable-pitch-headings t
           modus-%1$s-theme-rainbow-headings nil
           modus-%1$s-theme-section-headings t
           modus-%1$s-theme-scale-headings t
           modus-%1$s-theme-scale-1 1.1
           modus-%1$s-theme-scale-2 1.15
           modus-%1$s-theme-scale-3 1.21
           modus-%1$s-theme-scale-4 1.27
           modus-%1$s-theme-scale-5 1.33)
     (load-theme 'modus-%1$s t))
   theme))

(defun modus-themes-toggle ()
  "Toggle between `modus-operandi' and `modus-vivendi' themes."
  (interactive)
  (if (eq (car custom-enabled-themes) 'modus-operandi)
      (progn
        (disable-theme 'modus-operandi)
        (modus-vivendi-theme-load))
    (disable-theme 'modus-vivendi)
    (modus-operandi-theme-load)))

;; Light at sunrise
(modus-operandi-theme-load)
(load-theme 'modus-operandi t t)
(run-at-time (nth 1 (split-string (sunrise-sunset)))
             (* 60 60 24)
             (lambda ()
               (modus-operandi-theme-load)))

;; Dark at sunset
(load-theme 'modus-vivendi t t)
(run-at-time (nth 4 (split-string (sunrise-sunset)))
             (* 60 60 24)
             (lambda ()
               (modus-vivendi-theme-load)))

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
    (when (window-system)
      (message "toggling tool bar off")
      (tool-bar-mode -1))
    (when (window-system)
      (message "toggling scroll bar off")
      (toggle-scroll-bar -1))
    (line-number-mode t)
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
