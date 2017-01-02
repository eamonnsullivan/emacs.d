;; turn on syntax highlighting and highlighting of 
;; marked text. Turn off menu bar, toolbar, scrollbars and 
;; startup screen.

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

    ;; get a font size that's readable
    (set-face-attribute 'default nil :height 140)))

(require 'server)
(defadvice server-create-window-system-frame
  (after set-window-system-frame-colours ())
  "Set custom appearance settings when creating the first frame on a display"
  (message "Running after frame-initialize")
  (my-appearance-settings))
(ad-activate 'server-create-window-system-frame)
(add-hook 'after-make-frame-functions 'my-appearance-settings t)


