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
  (setopt trash-directory "~/.Trash")
  ;; See `trash-directory' as it requires defining `system-move-file-to-trash'.
  (defun system-move-file-to-trash (file)
    "Use \"trash\" to move FILE to the system trash."
    (cl-assert (executable-find "trash") nil "'trash' must be installed. Needs \"brew install trash\"")
    (call-process "trash" nil 0 nil "-F"  file))

  (when (display-graphic-p)
    (ns-raise-emacs))

  (setenv "SBT_OPTS" "-Xmx2G -Xms512M -Xss4M -XX:+UseG1GC -XX:+UseStringDeduplication -Dmetals.client=emacs -Djavax.net.ssl.trustStore=/etc/pki/jssecacerts -Djavax.net.ssl.trustStorePassword=changeit -Djavax.net.ssl.keyStore=/etc/pki/tls/private/client.p12 -Djavax.net.ssl.keyStorePassword=client -Djavax.net.ssl.keyStoreType=PKCS12 -Dfile.encoding=UTF-8"))



(use-package exec-path-from-shell
  :config
  (message "Initializing path from shell")
  (dolist (var '("GOPATH"
                 "SERVER_ENV"
                 "PATH"
                 "OP_SERVICE_ACCOUNT_TOKEN"
                 ))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

;; garbage collection tweaks
;; (setopt gc-cons-threshold (* 128 1024 1024))
;; (setopt read-process-output-max (* 1024 1024))

(provide 'init-platform)
