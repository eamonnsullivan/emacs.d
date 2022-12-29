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
        (setq lsp-metals-java-home "/usr/lib/jvm/java-8-openjdk-amd64")
        (message "configured for linux:"))
    (progn
      (lsp-register-custom-settings '(("metals.sbt-script" "/usr/local/bin/sbt")))
      (message "configured for Mac:"))))


(use-package lsp-mode
  :init
  (setq lsp-log-io nil
        lsp-lens-enable nil
        lsp-signature-auto-activate nil
        lsp-enable-indentation nil
        lsp-keymap-prefix "C-c l"
        lsp-prefer-flymake nil
        lsp-restart 'auto-restart
        lsp-pylsp-plugins-yapf-enabled t)
  :config
  ;; (dolist (m '(clojure-mode
  ;;              clojurec-mode
  ;;              clojurescript-mode
  ;;              clojurex-mode))
  ;;   (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  ;; lsp-clojure-server-command '("bash" "-c" "clojure-lsp")
  ;; (define-key lsp-mode-map (kbd "C-c l")
  (flycheck-add-next-checker 'scala 'scala-scalastyle)
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
    ("q" nil :color blue))
  :hook
  ((prog-mode . lsp-deferred)
   (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(define-key lsp-mode-map (kbd "C-c C-l") lsp-command-map)

(use-package treemacs
  :commands (treemacs)
  :after (lsp-mode))

;; workaround for https://github.com/emacs-lsp/lsp-metals/issues/84
(require 'treemacs-extensions)

(use-package lsp-metals)

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-header t
        lsp-ui-doc-include-signature nil
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-delay 0.05))

(use-package helm-lsp :commands helm-lsp-workspace-symbol)

;; Java support for lsp-mode using the Eclipse JDT Language Server.
(use-package lsp-java
  :hook (java-mode . lsp)
  :after lsp)

(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :commands lsp-treemacs-errors-list)

(use-package lsp-haskell
  :after lsp-mode
  :preface
  (defun slot/lsp-haskell-type-signature ()
    "Add a type signature for the thing at point.
This is very convenient, for example, when dealing with local
functions, since those—as opposed to top-level expressions—don't
have a code lens for \"add type signature here\" associated with
them."
    (interactive)
    (let* ((value (-some->> (lsp--text-document-position-params)
                    (lsp--make-request "textDocument/hover")
                    lsp--send-request
                    lsp:hover-contents
                    (funcall (-flip #'plist-get) :value))))
      (slot/back-to-indentation)
      (insert (slot/lsp-get-type-signature "haskell" value))
      (haskell-indentation-newline-and-indent)))

  ;; Fixes https://github.com/emacs-lsp/lsp-haskell/issues/151
  (cl-defmethod lsp-clients-extract-signature-on-hover
    (contents (_server-id (eql lsp-haskell)))
    "Display the type signature of the function under point."
    (message "%s" "lsp-haskell")
    (let* ((sig (slot/lsp-get-type-signature "haskell" (plist-get contents :value))))
      (lsp--render-element (concat "```haskell\n" sig "\n```"))))

  :bind (:map lsp-mode-map
              ("C-c C-t" . slot/lsp-haskell-type-signature))
  :config (add-hook 'lsp-lsp-haskell-after-open-hook
                    (lambda ()
                      (setq-local lsp-signature-render-documentation t))
                    nil t))

(eds/setup-sbt-lsp)

(provide 'init-lsp)
