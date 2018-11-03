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

(use-package
  cider
  :ensure t
  :hook ((clojure-mode . turn-on-eldoc-mode))
  :config
  (add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojurescript-mode)))

(provide 'init-lisp)
