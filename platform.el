;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; what kind of system are we using?  start with these, as it will influence
;; other stuff inspired by: http://www.xsteve.at/prg/emacs/.emacs.txt
;; There's something not quite right about the win32-p and linux-p. I haven't 
;; figured it out yet.
(defconst mac-p (string-equal system-type "darwin")
  "Are we running on MacOSX?")
(defconst linux-p (or (string-equal system-type "gnu/linux") (string-equal system-type "linux"))
  "Are we running on a GNU/Linux system?")
(defconst console-p (eq (symbol-value 'window-system) nil)
  "Are we running in a console (non-X) environment?")

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


