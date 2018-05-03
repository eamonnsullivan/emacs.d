;;; -*- lexical-binding: t -*-
;;; init-company.el --- stuff related to company mode

(use-package company
  :demand t
  :commands company-mode
  :config
  (global-company-mode)
  (setq company-global-modes '(not term-mode))
  (setq company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-show-numbers t
        company-tooltip-align-annotations t
        company-require-match nil
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil)
  ;; disables TAB in company-mode, freeing it for yasnippet
  (define-key company-active-map [tab] nil)
  (define-key company-active-map (kbd "TAB") nil)
  ;; Sort completion candidates that already occur in the current
  ;; buffer at the top of the candidate list.
  (setq company-transformers '(company-sort-by-occurrence))
  ;; Show documentation where available for selected completion
  ;; after a short delay.
  (use-package company-quickhelp
    :config
    (setq company-quickhelp-delay 1)
    (company-quickhelp-mode 1))

  (use-package company-tern
    :ensure t
    :init (add-to-list 'company-backends 'company-tern))

  (use-package company-go
    :ensure t
    :config
    (add-hook 'go-mode-hook (lambda ()
                              (set (make-local-variable 'company-backends) '(company-go))
                              (company-mode))))
  ;; Add a completion source for emoji. 😸
  (use-package company-emoji
    :config
    (company-emoji-init))
  (use-package company-try-hard
    :commands company-try-hard
    :bind ("C-\\" . company-try-hard)
    :config
    (bind-keys :map company-active-map
               ("C-\\" . company-try-hard)))
  :diminish company-mode)

(provide 'init-company)
