;;; -*- lexical-binding: t -*-
;; Platform-specific customizations
(when (featurep 'ns)
  (message "Running mac-specific initialization.")
  ;; Handle emacs server interactions on the Mac correctly.
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
    (ns-raise-emacs))

  ;; Seems to be needed for path to work on the mac
  (use-package exec-path-from-shell
    :ensure t
    :config
    (message "Initializing path from shell on Mac OS")
    (exec-path-from-shell-initialize)))

;; use local bin, if available
(if (file-directory-p "/usr/local/bin")
    (add-to-list 'exec-path "/usr/local/bin"))
(if (file-directory-p "/usr/local/bin")
    (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin")))
