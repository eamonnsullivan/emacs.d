;; Handle emacs server interactions on the Mac correctly.
(when (featurep 'ns)
  (defun ns-raise-emacs ()
    "Raise Emacs."
    (ns-do-applescript "tell application \"Emacs\" to activate"))

  (defun ns-raise-emacs-with-frame (frame)
    "Raise Emacs and select the provided frame."
    (with-selected-frame frame
      (when (display-graphic-p)
        (ns-raise-emacs))))

  (add-hook 'after-make-frame-functions 'ns-raise-emacs-with-frame)

  (when (display-graphic-p)
    (ns-raise-emacs)))

;; use local bin, if available
(if (file-directory-p "/usr/local/bin")
    (add-to-list 'exec-path "/usr/local/bin"))
(if (file-directory-p "/usr/local/bin")
    (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin")))

(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :ensure t
    :config
    (message "Initializing path from shell on Mac OS")
    (exec-path-from-shell-initialize)))


