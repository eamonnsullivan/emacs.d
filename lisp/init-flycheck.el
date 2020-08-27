;;; -*- lexical-binding: t -*-
;;; init-flycheck.el --- flycheck, setting various preferred checkers

(use-package flycheck-pos-tip
  :after flycheck)

(use-package flycheck
  :diminish flycheck-mode
  :config
  (global-flycheck-mode)
  ;; disable jshint since we prefer eslint checking
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  ;; use eslint with js2-mode for js and JSX files
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
  ;; customize flycheck temp file prefix
  (setq-default flycheck-temp-prefix ".flycheck")
  ;; disable json-jsonlist checking for json files
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(json-jsonlist)))
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(emacs-lisp-checkdoc)))
  (eval-after-load 'flycheck
  '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

(use-package flycheck-clj-kondo
  :if (executable-find "clj-kondo")
  :after clojure-mode
  :hook (clojure-mode . (lambda () (require 'flycheck-clj-kondo))))

(provide 'init-flycheck)
