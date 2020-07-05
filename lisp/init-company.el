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
        company-tooltip-align-annotations t))

(provide 'init-company)
