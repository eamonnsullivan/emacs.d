;;; -*- lexical-binding: t -*-
;;; init-company.el --- stuff related to company mode

(use-package company
  :demand t
  :commands company-mode
  :config
  (global-company-mode)
  (setopt company-global-modes '(not term-mode)
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-show-numbers t
        company-tooltip-align-annotations t)
  (add-to-list 'company-backends 'company-capf))

(provide 'init-company)
