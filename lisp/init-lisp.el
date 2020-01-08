;;; -*- lexical-binding: t -*-
;;; init-lisp.el --- stuff related to lisp programming

(use-package
  eldoc
  :commands eldoc-mode
  :diminish eldoc-mode
  :hook ((emacs-lisp-mode . turn-on-eldoc-mode)))

(use-package package-lint)

(require 'init-flycheck)

(use-package
  cider
  :hook ((clojure-mode . turn-on-eldoc-mode)
         (clojure-mode . cider-mode))
  :config
  (require 'flycheck-clj-kondo)
  (require 'init-org)
  (setq org-babel-clojure-backend 'cider)
  (setq cider-prompt-for-symbol nil)
  (setq cider-save-file-on-load t)
  (setq cider-font-lock-dynamically '(macro core function var))
  (setq cider-repl-pop-to-buffer-on-connect 'display-only)
  (setq
   cider-repl-history-file ".cider-repl-history"
   nrepl-log-messages t)
  (add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojurescript-mode))
  (flycheck-clojure-setup))

(use-package clj-refactor
  :hook ((clojure-mode . clj-refactor-mode)
         (clojure-mode . yas-minor-mode))
  :config
  (cljr-add-keybindings-with-prefix "C-c r"))

(use-package helm-cider
  :hook ((cider-mode . helm-cider-mode)))

(use-package cider-hydra
  :hook ((cider-mode . cider-hydra-mode))
  :bind ("C-c e" . cider-hydra-eval/body))

(use-package kibit-helper)

(provide 'init-lisp)
