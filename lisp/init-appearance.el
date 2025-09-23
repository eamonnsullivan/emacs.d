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

;; current faves
;; The themes are highly customisable.  Read the manual:
;; <https://protesilaos.com/emacs/modus-themes>.
(use-package modus-themes
  :straight
  (modus-themes :type git :host github :repo "protesilaos/modus-themes" :branch "main")
  :ensure t
  :demand t
  :config
  (setq modus-themes-custom-auto-reload nil
        ;; modus-themes-to-toggle '(modus-operandi modus-vivendi)
        modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted)
        ;; modus-themes-to-toggle '(modus-operandi-deuteranopia modus-vivendi-deuteranopia)
        ;; modus-themes-to-toggle '(modus-operandi-tritanopia modus-vivendi-tritanopia)
        modus-themes-to-rotate modus-themes-items
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui t
        modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-completions '((t . (bold)))
        modus-themes-prompts '(bold)
        modus-themes-common-palette-overrides nil
        modus-themes-headings
        '((agenda-structure . (variable-pitch light 2.2))
          (agenda-date . (variable-pitch regular 1.3))
          (t . (regular 1.15)))))

;; The themes are customisable.  Read the manual:
;; <https://protesilaos.com/emacs/ef-themes>.
(use-package ef-themes
  :straight
  (ef-themes :type git :host github :repo "protesilaos/ef-themes" :branch "main")
  :ensure t
  :demand t
  :config
  (setq ef-themes-variable-pitch-ui t
        ef-themes-mixed-fonts t
        ef-themes-to-rotate ef-themes-items
        ef-themes-headings ; read the manual's entry of the doc string
        '((0 . (variable-pitch light 1.9))
          (1 . (variable-pitch light 1.8))
          (2 . (variable-pitch regular 1.7))
          (3 . (variable-pitch regular 1.6))
          (4 . (variable-pitch regular 1.5))
          (5 . (variable-pitch 1.4)) ; absence of weight means `bold'
          (6 . (variable-pitch 1.3))
          (7 . (variable-pitch 1.2))
          (agenda-date . (semilight 1.5))
          (agenda-structure . (variable-pitch light 1.9))
          (t . (variable-pitch 1.1)))))


(defun modus-themes-toggle ()
  (if (eq (car custom-enabled-themes) 'modus-operandi-tinted)
      (modus-themes-load-theme 'modus-vivendi-tinted)
    (modus-themes-load-theme 'modus-operandi-tinted)))

;;;; Theme buffet
(use-package theme-buffet
  :straight t
  :after (:any modus-themes ef-themes)
  :defer 1
  :config
  (let ((modus-themes-p (featurep 'modus-themes))
        (ef-themes-p (featurep 'ef-themes)))
    (setq theme-buffet-menu 'end-user)
    (setq theme-buffet-end-user
          (cond
           ((and modus-themes-p ef-themes-p)
            '( :night
               (modus-vivendi ef-dark ef-winter ef-autumn ef-night ef-duo-dark ef-symbiosis)
               :morning
               (modus-operandi ef-light ef-cyprus ef-spring ef-frost ef-duo-light)
               :afternoon
               (modus-operandi-tinted ef-arbutus ef-day ef-kassio ef-summer ef-elea-light ef-maris-light ef-melissa-light ef-trio-light ef-reverie)
               :evening
               (modus-vivendi-tinted ef-rosa ef-elea-dark ef-maris-dark ef-melissa-dark ef-trio-dark ef-dream)))
           (ef-themes-p
            '( :night
               (ef-dark ef-winter ef-autumn ef-night ef-duo-dark ef-symbiosis ef-owl)
               :morning
               (ef-light ef-cyprus ef-spring ef-frost ef-duo-light ef-eagle)
               :afternoon
               (ef-arbutus ef-day ef-kassio ef-summer ef-elea-light ef-maris-light ef-melissa-light ef-trio-light ef-reverie)
               :evening
               (ef-rosa ef-elea-dark ef-maris-dark ef-melissa-dark ef-trio-dark ef-dream)))
           (modus-themes-p
            '( :night
               (modus-vivendi modus-vivendi-tinted)
               :morning
               (modus-operandi modus-operandi-tinted)
               :afternoon
               (modus-operandi modus-operandi-tinted)
               :evening
               (modus-vivendi modus-vivendi-tinted)))))

    (when (or modus-themes-p ef-themes-p)
      (theme-buffet-timer-hours 1))))

(use-package show-font
  :straight t
  :if (display-graphic-p)
  :commands (show-font-select-preview show-font-list show-font-tabulated)
  :config
  ;; These are the defaults, but I keep them here for easier access.
  (setq show-font-pangram 'prot)
  (setq show-font-character-sample
        "
ABCDEFGHIJKLMNOPQRSTUVWXYZ
abcdefghijklmnopqrstuvwxyz
0123456789   !@#$¢%^&*~|
`'\"‘’“”.,;:  ()[]{}—-_+=<>

()[]{}<>«»‹› 6bB8&0ODdoa 1tiIlL|\/
!ij c¢ 5$Ss 7Z2z 9gqp nmMNNMW uvvwWuuw
x×X .,·°;:¡!¿?`'‘’   ÄAÃÀ TODO
")
  (setq show-font-display-buffer-action-alist '(display-buffer-full-frame)))


(defun disable-all-themes ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))



(defun my-appearance-settings (&rest frame)
  "Apply my preferences on graphical appearance."
  (interactive "P")
  (progn
    (message "my-appearance-settings running.")
    (transient-mark-mode t)
    (menu-bar-mode -1)
    (when (window-system)
      (message "toggling scroll bar off")
      (toggle-scroll-bar -1))
    (line-number-mode t)
    (display-time-mode t)
    (setq display-time-24hr-format t)

    (if (not (find-font (font-spec :name "Aporetic Sans Mono")))
        (message "Font Aporetic Sans Mono not found, using default font settings"))
    (theme-buffet-a-la-carte)))




(provide 'init-appearance)
