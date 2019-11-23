
;;; -*- lexical-binding: t -*-
;;; init-lsp.el --- stuff related to the language server protocol

(straight-use-package 'lsp-mode)

(use-package lsp-mode
  :config
  (add-to-list 'lsp-language-id-configuration '(clojure-mode . "clojure-mode"))
  :hook
  ;; npm i -g typescript-language-server; npm i -g typescript
  (typescript-mode . lsp)
  (js2-mode . lsp)
  (rjsx-mode . lsp)
  (java-mode . lsp)
  (scala-mode . lsp)
  ;; https://github.com/snoe/clojure-lsp/releases/tag/release-20191010T151127
  ;; https://github.com/snoe/clojure-lsp/releases/latest
  ;; (clojure-mode . lsp)
  ;; (clojurec-mode . lsp)
  ;; (clojurescript-mode . lsp)
  ;; npm install -g vscode-css-languageserver-bin
  (css-mode . lsp)
  :commands lsp
  :init
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-snippet t)
  (setq lsp-auto-configure t)
  (setq lsp-enable-xref t)
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-on-type-formatting nil))

(use-package lsp-ui
  :commands lsp-ui-mode
  ;; :config (setq lsp-ui-doc-enable nil) ;; workaround for https://github.com/emacs-lsp/lsp-ui/issues/299
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)))
(use-package company-lsp :commands company-lsp)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)

;; Java support for lsp-mode using the Eclipse JDT Language Server.
(use-package lsp-java
  :after lsp)
(use-package dap-mode
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))
  ;; :config
  ;; (setq lsp-java-save-action-organize-imports nil)
  ;; (setq lsp-java-organize-imports nil))


(use-package lsp-pwsh
  :straight (lsp-pwsh
             :host github
             :repo "kiennq/lsp-powershell")
  :hook (powershell-mode . (lambda () (require 'lsp-pwsh) (lsp)))
  :defer t)

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(provide 'init-lsp)
