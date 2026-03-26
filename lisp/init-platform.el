;;; init-platform.el --- Platform initialisation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan <me@eamonnsullivan.co.uk>

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Maintainer: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Created: 2017-01-02
;; Version: 0.1
;; Package-Requires: ((emacs "24.4"))
;; Keywords: platform, environment, shell, tools
;; URL: https://github.com/eamonnsullivan/init-platform

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides initialisation and configuration for platform-specific
;; environment handling in Emacs, including support for exec-path-from-shell.

;;; Licence:

;; This programme is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public Licence as published by
;; the Free Software Foundation, either version 3 of the Licence, or
;; (at your option) any later version.

;; This programme is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public Licence for more details.

;; You should have received a copy of the GNU General Public Licence
;; along with this programme.  If not, see <https://www.gnu.org/licenses/>.

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
    (ns-raise-emacs)
    (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

  ;; I may regret this, but let's try and join the crowd and swap the
  ;; key on the mac. It's all in the Meta. I got this and the function from
  ;; https://github.com/bbatsov/prelude/blob/master/core/prelude-macos.el
  (setq ns-function-modifier 'hyper)

  (defun prelude-swap-meta-and-super ()
    "Swap the mapping of Meta and Super.
Very useful for people using their Mac with a
Windows external keyboard from time to time."
    (interactive)
    (if (eq mac-command-modifier 'super)
        (progn
          (setq mac-command-modifier 'meta)
          (setq mac-option-modifier 'super)
          (message "Command is now bound to META and Option is bound to SUPER."))
      (setq mac-command-modifier 'super)
      (setq mac-option-modifier 'meta)
      (message "Command is now bound to SUPER and Option is bound to META.")))

  ;; Let me change my mind, or while using my ergonomic keyboard.
  (global-set-key (kbd "C-c w") 'prelude-swap-meta-and-super)

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




(provide 'init-platform)
;;; init-platform.el ends here
