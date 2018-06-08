;;; -*- lexical-binding: t -*-
;;; init-lsp.el --- stuff related to the language server protocol

(use-package lsp-mode
  :diminish lsp-mode
  :config
  (use-package lsp-ui
    :commands (lsp-ui-mode lsp-ui-peek-find-definitions lsp-ui-peek-find-references)
    :bind (:map lsp-ui-mode-map
                ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
                ([remap xref-find-references] . lsp-ui-peek-find-references))
    :init (add-hook 'lsp-mode-hook 'lsp-ui-mode))

  (use-package company-lsp
    :init (add-to-list 'company-backends 'company-lsp)))

;; Javascript, Typescript and Flow support for lsp-mode
;;
;; Install: npm i -g javascript-typescript-langserver
;;
;; I'm going to use this only for typescript to start with. It gets in
;; the way in other modes.
(use-package lsp-javascript-typescript
  :commands lsp-javascript-typescript-enable
  :init
  (add-hook 'typescript-mode-hook #'lsp-javascript-typescript-enable))

;; Java support for lsp-mode using the Eclipse JDT Language Server.
;;
;; Install:
;; wget http://download.eclipse.org/jdtls/snapshots/jdt-language-server-latest.tar.gz
;; tar xvf jdt-language-server-latest.tar.gz -C ~/.emacs.d/eclipse.jdt.ls/server/
;; I'm not overly impressed at first glance, but giving it a shot.
(use-package lsp-java
  :commands lsp-java-enable
  :config
  (setq lsp-java-save-action-organize-imports nil)
  (setq lsp-java-organize-imports nil)
  :init
  (add-hook 'java-mode-hook #'lsp-java-enable)
  (setq lsp-java--workspace-folders (list "~/git/cps-breaking-news")))

(provide 'init-lsp)
