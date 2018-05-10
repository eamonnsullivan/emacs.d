;;; -*- lexical-binding: t -*-
;; init-platform.el -- Platform-specific customizations

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
    :if (and (eq system-type 'darwin) (display-graphic-p))
    :ensure t
    :config
    (message "Initializing path from shell on Mac OS")
    (dolist (var '("GOPATH"
                   "SERVER_ENV"
                   ))
      (add-to-list 'exec-path-from-shell-variables var))
    (exec-path-from-shell-initialize)))

;; use local bin, if available
(if (file-directory-p "/usr/local/bin")
    (add-to-list 'exec-path "/usr/local/bin"))
(if (file-directory-p "/usr/local/bin")
    (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin")))

;; garbage collection tweaks
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'after-init-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(provide 'init-platform)
