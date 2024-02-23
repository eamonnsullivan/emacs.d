;;; -*- lexical-binding: t -*-
;; init-platform.el -- Platform-specific customizations

(when (featurep 'ns)
  (message "Running mac-specific initialization.")
  ;; Handle emacs server interactions on the Mac correctly.
  ;; (setenv "LIBRARY_PATH" "/usr/local/Cellar/gcc/11.2.0_3/lib/gcc/11:/usr/local/opt/libgccjit/lib/gcc/11")
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

(use-package exec-path-from-shell
  :config
  (message "Initializing path from shell")
  (dolist (var '("GOPATH"
                 "SERVER_ENV"
                 "PATH"
                 ))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

;; garbage collection tweaks
;; (setq gc-cons-threshold (* 128 1024 1024))
;; (setq read-process-output-max (* 1024 1024))

(provide 'init-platform)
