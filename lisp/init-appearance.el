;;; -*- lexical-binding: t -*-
;;; init-appearance.el --- Code related to the look of emacs

(use-package material-theme)
;;  :config (load-theme 'material-light t))

(use-package parchment-theme)
  ;; :ensure t
  ;; :config (load-theme 'parchment t))

(use-package poet-theme)
  ;; :init
  ;; (add-hook 'text-mode-hook
  ;;          (lambda ()
  ;;            (variable-pitch-mode 1)))
  ;; :config
  ;; (load-theme 'poet t))

(use-package doom-themes)
  ;; :config
  ;; (load-theme 'doom-one-light t))

(use-package zenburn-theme)
  ;; :config
  ;; (load-theme 'zenburn t))

(use-package leuven-theme
  :straight
  (leuven-theme :type git :host github :repo "fniessen/emacs-leuven-theme")
 :config
 (load-theme 'leuven t))

(use-package modus-vivendi-theme
  :config
  ;; Choose to render more code constructs in slanted text (italics).  The
  ;; default, shown below, is to not use italics, unless it is absolutely
  ;; necessary.
  (setq modus-vivendi-theme-slanted-constructs nil)

  ;; Opt to display some additional code constructs in bold.  The default,
  ;; shown below, is to use bold weight only where necessary.
  (setq modus-vivendi-theme-bold-constructs nil)

  ;; Use proportionately-spaced fonts (variable-pitch) for headings.  The
  ;; default is to use whatever font the user has selected, typically a
  ;; monospaced typeface.
  (setq modus-vivendi-theme-proportional-fonts nil)

  ;; Whether headings should be scaled or have the same height as body
  ;; text.  The default is to keep everything the same as the base size.
  (setq modus-vivendi-theme-scale-headings nil)

  ;; Font scale that should apply to headings.  These are the default values.
  (setq modus-vivendi-theme-scale-1 1.05)
  (setq modus-vivendi-theme-scale-2 1.1)
  (setq modus-vivendi-theme-scale-3 1.15)
  (setq modus-vivendi-theme-scale-4 1.2))


(use-package modus-operandi-theme
  :config
  ;; Choose to render more code constructs in slanted text (italics).  The
  ;; default, shown below, is to not use italics, unless it is absolutely
  ;; necessary.
  (setq modus-operandi-theme-slanted-constructs nil)

  ;; Opt to display some additional code constructs in bold.  The default,
  ;; shown below, is to use bold weight only where necessary.
  (setq modus-operandi-theme-bold-constructs nil)

  ;; Use proportionately-spaced fonts (variable-pitch) for headings.  The
  ;; default is to use whatever font the user has selected, typically a
  ;; monospaced typeface.
  (setq modus-operandi-theme-proportional-fonts nil)

  ;; Whether headings should be scaled or have the same height as body
  ;; text.  The default is to keep everything the same as the base size.
  (setq modus-operandi-theme-scale-headings nil)

  ;; Font scale that should apply to headings.  These are the default values.
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
    (global-font-lock-mode 1)
    (setq font-lock-maximum-decoration t)
    (transient-mark-mode t)
    (menu-bar-mode -1)
    (when (window-system)
      (message "toggling tool bar off")
      (tool-bar-mode -1))
    (when (window-system)
      (message "toggling scroll bar off")
      (toggle-scroll-bar -1))
    (setq inhibit-startup-message t)
    (line-number-mode t)                      ; show line numbers
    (column-number-mode t)                    ; show column numbers
    (when-available 'size-indication-mode
                    (size-indication-mode t)) ; show file size (emacs 22+)
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
