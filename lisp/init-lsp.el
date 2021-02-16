;;; -*- lexical-binding: t -*-
;;; init-lsp.el --- stuff related to the language server protocol

(require 'init-hydra)

;; Check that this applied with (lsp-configuration-section "metals")
;; Check also that you are using Java 8. Things break in Java 11.
(defun eds/setup-sbt-lsp ()
  (message "Configuring LSP on %s" system-type)
  (if (eq system-type 'gnu/linux)
      (progn
        (lsp-register-custom-settings '(("metals.sbt-script" "/usr/bin/sbt")))
        (message "configured for linux:"))
    (progn
      (lsp-register-custom-settings '(("metals.sbt-script" "/usr/local/bin/sbt")))
      (message "configured for Mac:"))))

(use-package lsp-mode
  :init
  (setq lsp-log-io nil
        lsp-lens-enable nil
        lsp-signature-auto-activate nil)
  :config
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  ;; lsp-clojure-server-command '("bash" "-c" "clojure-lsp")
  (setq lsp-enable-indentation nil)
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
  :commands (lsp lsp-deferred))

(use-package lsp-metals)

(use-package lsp-ui)

(use-package helm-lsp
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)
  :commands helm-lsp-workspace-symbol)

;; Java support for lsp-mode using the Eclipse JDT Language Server.
(use-package lsp-java
  :hook (java-mode . lsp)
  :after lsp)

(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :commands lsp-treemacs-errors-list)

(use-package treemacs
  :commands (treemacs)
  :after (lsp-mode))

(eds/setup-sbt-lsp)

(provide 'init-lsp)
