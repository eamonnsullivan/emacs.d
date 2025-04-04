;;; -*- lexical-binding: t -*-
;;; init-flycheck.el --- flycheck, setting various preferred checkers

(use-package flycheck-pos-tip
  :after flycheck)

(use-package flycheck
  :diminish flycheck-mode
  :config
  ;; disable jshint since we prefer eslint checking
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  ;; use eslint with js2-mode for js and JSX files
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
  (if (eq system-type 'darwin)
      (setq flycheck-javascript-eslint-executable "/Users/sullie09/.nvm/versions/node/v16.19.1/bin/eslint_d")
    (setq flycheck-javascript-eslint-executable "/home/eamonn/.nvm/versions/node/v16.19.1/bin/eslint_d"))
  ;; add scalastyle for scala mode.
  (flycheck-add-mode 'scala-scalastyle 'scala-mode)
  ;; customize flycheck temp file prefix
  (setq-default flycheck-temp-prefix ".flycheck")
  ;; disable json-jsonlist checking for json files
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(json-jsonlist)))
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(emacs-lisp-checkdoc)))
  (setq-default flycheck-scalastylerc "/home/eamonn/git/disco-api/scalastyle-config.xml")
  (eval-after-load 'flycheck
    '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
  :init   (global-flycheck-mode))

(use-package flycheck-clj-kondo
  :if (executable-find "clj-kondo")
  :after clojure-mode
  :hook (clojure-mode . (lambda () (require 'flycheck-clj-kondo))))

(provide 'init-flycheck)
