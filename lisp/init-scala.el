;;; -*- lexical-binding: t -*-
;;; init-scala.el --- stuff related to coding in scala

(use-package scala-mode
  :config
  (setq scala-indent:default-run-on-strategy
        scala-indent:operator-strategy))

(require 'eds)
(use-package sbt-mode
  :commands sbt-start sbt-command
  :bind (("C-c C-b" . sbt-hydra)
         ("C-c t" . eds/annotate-scala-symbol-with-type))
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

;; feature-mode
(use-package feature-mode
  :config
  (defvar feature-step-search-path "src/test/scala/steps/**/*Steps.scala"))

(provide 'init-scala)
