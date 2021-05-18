;;; -*- lexical-binding: t -*-
;;; init-term.el --- ansi terminal code

;; visit-term
;;
;; I'm using libvterm now, which basically breaks this
;; functionality. You can't run C-<f2> again while in a vterm window.
(defun visit-term ()
  "If the current buffer is:
     1) a running vterm named vterm, rename it.
     3) a non vterm, go to an already running vterm
        or start a new one."
  (interactive)
  (let ((is-term (string= "vterm-mode" major-mode))
        (anon-term (get-buffer "vterm")))
    (if is-term
        (if (string= "vterm" (buffer-name))
            (call-interactively 'rename-buffer)
          (if anon-term
              (switch-to-buffer "vterm")
            (vterm)))
      (if anon-term
          (switch-to-buffer "vterm")
        (vterm)))))

(global-set-key (kbd "C-<f2>") 'visit-term)

(use-package vterm
  :straight
  (vterm :type git :host github :repo "akermu/emacs-libvterm")
  :bind ("C-<f2>" . visit-term))


(provide 'init-term)
