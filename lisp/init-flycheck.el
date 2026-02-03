;;; -*- lexical-binding: t -*-
;;; init-flycheck.el --- flycheck, setting various preferred checkers

(use-package flycheck-pos-tip
  :after flycheck)

(use-package flycheck
  :straight t
  :config
  (flycheck-add-mode 'scala-scalastyle 'scala-ts-mode)
  :init
  (global-flycheck-mode))

(use-package flycheck-eglot
  :straight
  (:host github :repo "flycheck/flycheck-eglot" :files ("*.el"))
  :after (flycheck eglot)
  :custom (flycheck-eglot-exclusive nil)
  :config
  (global-flycheck-eglot-mode 1))

(use-package flycheck-clj-kondo
  :if (executable-find "clj-kondo")
  :after clojure-mode
  :hook (clojure-mode . (lambda () (require 'flycheck-clj-kondo))))

(provide 'init-flycheck)
