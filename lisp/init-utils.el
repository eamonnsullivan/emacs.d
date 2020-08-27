;;; -*- lexical-binding: t -*-
;;; init_utils.el --- General utility code. Stuff I didn't know where else to put.

(use-package esup
  :defer t)

(use-package helpful
  :bind
  (([remap describe-function] . helpful-callable)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-key] . helpful-key)
   :map emacs-lisp-mode-map
   ("C-c C-d" . helpful-at-point)))

(provide 'init-utils)
