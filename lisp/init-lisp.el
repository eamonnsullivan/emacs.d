;;; -*- lexical-binding: t -*-
;;; init-lisp.el --- stuff related to lisp programming

(use-package
  eldoc
  :commands eldoc-mode
  :diminish eldoc-mode
  :hook ((emacs-lisp-mode . turn-on-eldoc-mode)))

(use-package edn)

(use-package package-lint)

(require 'init-flycheck)

(use-package
  cider
  :hook ((clojure-mode . turn-on-eldoc-mode)
         (clojure-mode . cider-mode))
  :config
  (require 'flycheck-clj-kondo)
  (dolist (checker '(clj-kondo-clj clj-kondo-cljs clj-kondo-cljc clj-kondo-edn))
    (setq flycheck-checkers (cons checker (delq checker flycheck-checkers))))
  (require 'init-org)
  (setq org-babel-clojure-backend 'cider
        cider-prompt-for-symbol nil
        cider-save-file-on-load t
        cider-font-lock-dynamically '(macro core function var)
        cider-repl-pop-to-buffer-on-connect 'display-only
        cider-overlays-use-font-lock t
        nrepl-hide-special-buffers t
        cider-repl-history-file ".cider-repl-history")
  (add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojurescript-mode)))

(use-package clj-refactor
  :hook ((clojure-mode . clj-refactor-mode)
         (clojure-mode . yas-minor-mode))
  :config
  (cljr-add-keybindings-with-prefix "C-c r")
  (setq cljr-warn-on-eval nil))

(use-package helm-cider
  :hook ((cider-mode . helm-cider-mode)))

(use-package cider-hydra
  :hook ((cider-mode . cider-hydra-mode))
  :bind ("C-c e" . cider-hydra-eval/body))

(use-package kibit-helper)

(provide 'init-lisp)
