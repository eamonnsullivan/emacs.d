;;; -*- lexical-binding: t -*-
;; platform.el --- Platform-specific customizations

;; Copyright (c) 2017 Eamonn Sullivan

;; Author: Eamonn Sullivan <eamonn.sullivan@gmail.com>
;; Maintainer: Eamonn Sullivan <eamonn.sullivan@gmail.com>
;; Created 23 March 2017

;; Homepage: https://github.com/eamonnsullivan/emacs.d

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;;; Code:

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

;;; platform.el ends here
