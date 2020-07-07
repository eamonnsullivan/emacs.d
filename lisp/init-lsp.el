;;; -*- lexical-binding: t -*-
;;; init-lsp.el --- stuff related to the language server protocol

(require 'init-hydra)

;; Check that this applied with (lsp-configuration-section "metals")
;; Check also that you are using Java 8. Things break in Java 11.
(defun eds/setup-sbt-lsp ()
  (message "Configuring LSP on %s" system-type)
  (if (eq system-type 'gnu/linux)
      (progn
        (message "configured for linux")
        (lsp-register-custom-settings '(("metals.sbt-script" "/usr/bin/sbt"))))
    (progn
      (message "configured for Mac")
      (lsp-register-custom-settings '(("metals.sbt-script" "/usr/local/bin/sbt"))))))


(use-package lsp-mode
  :config
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
     (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  (setq lsp-enable-indentation nil
        lsp-clojure-server-command '("bash" "-c" "clojure-lsp"))
  (eds/setup-sbt-lsp)
  (define-key lsp-mode-map (kbd "C-c l")
    (defhydra hydra-lsp (:color red :hint nil)
   "
^Symbols^                ^Actions^
^^^^^^^^^-----------------------------------------------------
_d_: Find definition     _s_: Shutdown language server
_u_: Find usages         _r_: open buffer on desktop
_n_: Rename symbol

_q_: quit this menu
"
      ("d" lsp-find-definition)
      ("u" lsp-ui-peek-find-references)
      ("c" lsp-treemacs-call-hierarchy)
      ("n" lsp-rename)
      ("s" lsp-workspace-shutdown)
      ("r" lsp-workspace-restart)
      ("q" nil :color blue)))
  :hook
  (prog-mode . lsp-deferred)
  (lsp-mode . lsp-lens-mode)
  (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp
  :init
  (setq lsp-log-io nil))

(use-package lsp-metals)

(use-package lsp-ui
  :commands lsp-ui-mode
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
  :hook
  (lsp-mode . lsp-ui-mode))

(use-package helm-lsp
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)
  :commands helm-lsp-workspace-symbol)

;; Java support for lsp-mode using the Eclipse JDT Language Server.
(use-package lsp-java
  :after lsp)

(use-package lsp-treemacs)

(provide 'init-lsp)
