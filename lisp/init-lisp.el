;;; -*- lexical-binding: t -*-
;;; init-lisp.el --- stuff related to lisp programming

(use-package
  eldoc
  :commands eldoc-mode
  :diminish eldoc-mode
  :hook ((emacs-lisp-mode . turn-on-eldoc-mode)))

(use-package
  lispy
  :config (add-hook
           'emacs-lisp-mode-hook
           (lambda () (lispy-mode 1))))

(provide 'init-lisp)
