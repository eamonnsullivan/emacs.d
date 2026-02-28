;;; init-kbd-macros.el --- stuff related to custom keyboard macros. -*- lexical-binding: t -*-
;;
;; Copyright (C) 2026 Eamonnn Sullivan
;;
;;
;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Version: 1.0
;; Keywords: emacs macros
;; URL: https://eamonnsullivan.co.uk
;;
;;
;;; Commentary:
;;
;;
;;; Code:

;; Got this code from https://www.emacswiki.org/emacs/KeyboardMacros
(defun eds/macro-query (arg)
  "Prompt for input using the minibuffer during kbd macro execution.
With prefix ARG, allows you to select what prompt string to use.
If the input is non-empty, it is inserted at point."
  (interactive "P")
  (let* ((prompt (if arg (read-from-minibuffer "PROMPT: ") "Input: "))
         (input (minibuffer-with-setup-hook (lambda () (kbd-macro-query t))
                  (read-from-minibuffer prompt))))
    (unless (string= "" input) (insert input))))

(global-set-key "\C-xQ" 'eds/macro-query)

(provide 'init-kbd-macros)
