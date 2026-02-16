;;; -*- lexical-binding: t -*-
;;; init-presentations.el --- stuff for presentations.
;;; Largely lifted from https://ankit.earth/blog/my-emacs-presentation-stack/

(use-package logos
  :init
  ;; Always expand the current Heading.
  (defun arg-reveal-entry ()
	"Reveal Org or Outline entry."
	(cond
	 ((and (eq major-mode 'org-mode)
           (org-at-heading-p))
      (org-show-entry))
	 ((or (eq major-mode 'outline-mode)
          (bound-and-true-p outline-minor-mode))
      (outline-show-entry))))
  :custom
  (logos-outlines-are-pages t)
  (logos-olivetti t)
  :hook
  (logos-page-motion . arg-reveal-entry)
  :bind
  ;; Binds Logos functions for Page motions.
  (:map org-mode-map
		([remap forward-page] . logos-forward-page-dwim)
		([remap backward-page] . logos-backward-page-dwim))
  :config
  ;; Disable distracting elements during Focus mode.
  (setq-default logos-hide-mode-line t
				logos-hide-header-line t))

(use-package olivetti
  :config
  ;; Fancy mode uses margins and fringes to create vertical
  ;; blocks on either sides of the content.
  (setopt olivetti-style 'fancy)
  (setq-default olivetti-body-width 0.4)
  (add-hook 'olivetti-mode-on-hook
			(lambda ()
			  ;; Disable Line numbers.
			  (display-line-numbers-mode -1)
			  ;; Disable Buffer boundaries.
			  (setq-local indicate-buffer-boundaries nil)))
  (add-hook 'olivetti-mode-off-hook
			(lambda ()
			  ;; Enable Line numbers.
			  (display-line-numbers-mode 1)
			  ;; Restores Buffer boundaries.
			  (setq-local indicate-buffer-boundaries 'left))))


(defun eds/start-presentation ()
  "Start Presentation - Configures Frame, Style, etc."
  (interactive)
  (when (eq major-mode 'org-mode)
    (setq-local arg-presentation t)
    (logos-focus-mode 1)
    (logos-narrow-dwim)
    (org-fold-show-entry)
    (toggle-frame-tab-bar (selected-frame))
    (menu-bar-mode -1)))

(defun eds/stop-presentation ()
  "Stop Presentation - Reverts Frame, Style, etc."
  (interactive)
  (cond ((not (eq major-mode 'org-mode))
         (error "This command only works in Org buffers."))
        ((not arg-presentation)
         (error "Not presenting right now."))
        (t (progn
             (logos-focus-mode -1)
             (widen)
             (toggle-frame-tab-bar (selected-frame))
             (setq-local arg-presentation nil)
             (menu-bar-mode +1)))))

(provide 'init-presentations)

;;; org.el ends here
