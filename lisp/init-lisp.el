;;; -*- lexical-binding: t -*-
;;; init-lisp.el --- stuff related to lisp programming

(use-package
  eldoc
  :commands eldoc-mode
  :diminish eldoc-mode
  :hook ((emacs-lisp-mode . turn-on-eldoc-mode)))

(use-package package-lint)

(use-package
  cider
  :hook ((clojure-mode . turn-on-eldoc-mode)
         (clojure-mode . cider-mode))
  :config
  (add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojurescript-mode)))

(use-package
  clj-refactor
  :config
  (defun eds-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1)
    (cljr-add-keybindings-with-prefix "C-c C-m"))
  (add-hook 'clojure-mode-hook #'eds-clojure-mode-hook))

(provide 'init-lisp)
