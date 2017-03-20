;; turn on syntax highlighting and highlighting of
;; marked text. Turn off menu bar, toolbar, scrollbars and
;; startup screen.

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
  (progn
    (message "my-appearance-settings running.")
    (global-font-lock-mode 1)
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
    (when (window-system)
      (message "setting foreground and background colours")
      (set-background-color "White")
      (set-foreground-color "Black"))
    (when (window-system)
      (message "setting default-frame-alist")
      (setq default-frame-alist
            '((vertical-scroll-bars)
              (tool-bar-lines . 0)
              (menu-bar-lines . 0)
              (menu-bar-mode . -1)
              (tool-bar-mode . -1)
              (toggle-scroll-bar . -1)
              (background-color . "White")
              (foreground-color . "Black"))))

    ;; set some font options, if the fonts are available
    (if (and (find-font (font-spec :name "Hack"))
             (find-font (font-spec :name "Fira Sans")))
        (when window-system
          (if (> (x-display-pixel-width) 1800)
              (my-setup-main-fonts 160 170)
            (my-setup-main-fonts 150 160)))
      (set-face-attribute 'default nil :height 140))))


(require 'server)
(defadvice server-create-window-system-frame
  (after set-window-system-frame-colours ())
  "Set custom appearance settings when creating the first frame on a display"
  (message "Running after frame-initialize")
  (my-appearance-settings))
(ad-activate 'server-create-window-system-frame)
(add-hook 'after-make-frame-functions 'my-appearance-settings t)
