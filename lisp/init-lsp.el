
;;; -*- lexical-binding: t -*-
;;; init-lsp.el --- stuff related to the language server protocol

(straight-use-package 'lsp-mode)

(use-package lsp-mode
  :hook
  ;; npm i -g typescript-language-server; npm i -g typescript
  (typescript-mode . lsp)
  (js2-mode . lsp)
  (rjsx-mode . lsp)
  (java-mode . lsp)
  (scala-mode . lsp)
  ;; npm install -g vscode-css-languageserver-bin
  (css-mode . lsp)
  :commands lsp
  :init
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-snippet t)
  (setq lsp-auto-configure t)
  (setq lsp-enable-xref t))

(use-package lsp-ui
  :commands lsp-ui-mode
  ;; :config (setq lsp-ui-doc-enable nil) ;; workaround for https://github.com/emacs-lsp/lsp-ui/issues/299
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)))
(use-package company-lsp :commands company-lsp)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)

;; Java support for lsp-mode using the Eclipse JDT Language Server.
;;
;; Install:
;; wget http://download.eclipse.org/jdtls/snapshots/jdt-language-server-latest.tar.gz
;; tar xvf jdt-language-server-latest.tar.gz -C ~/.emacs.d/eclipse.jdt.ls/server/
(use-package lsp-java
  :config
  (setq lsp-java-save-action-organize-imports nil)
  (setq lsp-java-organize-imports nil)
  :init
  (add-hook 'java-mode-hook 'lsp)
  (setq lsp-java--workspace-folders (list "~/git/cps-breaking-news")))

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(provide 'init-lsp)
