;;; -*- lexical-binding: t -*-
;;; init-appearance.el --- Code related to the look of emacs


(use-package fontaine
  :straight t
  :config
  (setq fontaine-latest-state-file (locate-user-emacs-file "fontaine-latest-state.eld")
        fontaine-presets
        '((small
           :default-family "Aporetic Serif Mono"
           :default-height 80
           :variable-pitch-family "Aporetic Sans")
          (regular) ; like this it uses all the fallback values and is named `regular'
          (medium
           :default-weight semilight
           :default-height 115
           :bold-weight extrabold)
          (large
           :inherit medium
           :default-height 150)
          (presentation
           :default-height 180)
          (t
           ;; I keep all properties for didactic purposes, but most can be
           ;; omitted.  See the fontaine manual for the technicalities:
           ;; <https://protesilaos.com/emacs/fontaine>.
           :default-family "Aporetic Sans Mono"
           :default-weight regular
           :default-height 100

           :fixed-pitch-family nil ; falls back to :default-family
           :fixed-pitch-weight nil ; falls back to :default-weight
           :fixed-pitch-height 1.0

           :fixed-pitch-serif-family nil ; falls back to :default-family
           :fixed-pitch-serif-weight nil ; falls back to :default-weight
           :fixed-pitch-serif-height 1.0

           :variable-pitch-family "Aporetic Serif"
           :variable-pitch-weight nil
           :variable-pitch-height 1.0

           :mode-line-active-family nil ; falls back to :default-family
           :mode-line-active-weight nil ; falls back to :default-weight
           :mode-line-active-height 0.9

           :mode-line-inactive-family nil ; falls back to :default-family
           :mode-line-inactive-weight nil ; falls back to :default-weight
           :mode-line-inactive-height 0.9

           :header-line-family nil ; falls back to :default-family
           :header-line-weight nil ; falls back to :default-weight
           :header-line-height 0.9

           :line-number-family nil ; falls back to :default-family
           :line-number-weight nil ; falls back to :default-weight
           :line-number-height 0.9

           :tab-bar-family nil ; falls back to :default-family
           :tab-bar-weight nil ; falls back to :default-weight
           :tab-bar-height 1.0

           :tab-line-family nil ; falls back to :default-family
           :tab-line-weight nil ; falls back to :default-weight
           :tab-line-height 1.0

           :bold-family nil ; use whatever the underlying face has
           :bold-weight bold

           :italic-family nil
           :italic-slant italic

           :line-spacing nil)))
  ;; Set the last preset or fall back to desired style from `fontaine-presets'
  ;; (the `regular' in this case).
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'large))

  ;; Persist the latest font preset when closing/starting Emacs and
  ;; while switching between themes.
  (fontaine-mode 1))

(defun disable-all-themes ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

;; current fave
(use-package modus-themes
  :straight
  (modus-themes :type git :host gitlab :repo "protesilaos/modus-themes" :branch "main")
  :init
  (setq modus-themes-mixed-fonts t)
  :config
  (modus-themes-load-theme 'modus-operandi-tinted)
  :demand t)

(defun modus-themes-toggle ()
  (if (eq (car custom-enabled-themes) 'modus-operandi-tinted)
      (modus-themes-load-theme 'modus-vivendi-tinted)
    (modus-themes-load-theme 'modus-operandi-tinted)))

;; Light at sunrise
(run-at-time (nth 1 (split-string (sunrise-sunset)))
             (* 60 60 24)
             (lambda ()
               (modus-themes-load-theme 'modus-operandi-tinted)))

;; Dark at sunset
(run-at-time (nth 4 (split-string (sunrise-sunset)))
             (* 60 60 24)
             (lambda ()
               (modus-themes-load-theme 'modus-vivendi-tinted)))

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

    (if (not (find-font (font-spec :name "Aporetic Sans Mono")))
        (message "Font Aporetic Sans Mono not found, using default font settings"))))


(provide 'init-appearance)
