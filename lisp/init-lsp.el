;;; -*- lexical-binding: t -*-
;;; init-lsp.el --- stuff related to the language server protocol

(straight-use-package 'lsp-mode)

(defun eds/setup-sbt-lsp ()
  (message "Configuring LSP")
  (if (eq system-type 'gnu/linux)
      (lsp-register-custom-settings '(("metals.sbt-script" "/usr/bin/sbt")))
    (lsp-register-custom-settings '(("metals.sbt-script" "/usr/local/bin/sbt")))))

(use-package lsp-mode
  :config
  (add-to-list 'lsp-language-id-configuration '(clojure-mode . "clojure-mode"))
  (eds/setup-sbt-lsp)
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
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-log-io nil))

(use-package lsp-ui
  :commands lsp-ui-mode
  ;; :config (setq lsp-ui-doc-enable nil) ;; workaround for https://github.com/emacs-lsp/lsp-ui/issues/299
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
  :hook
  (lsp-mode . lsp-ui-mode))

(use-package company-lsp :commands company-lsp)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)

;; Java support for lsp-mode using the Eclipse JDT Language Server.
(use-package lsp-java
  :after lsp)


(provide 'init-lsp)
