;;; -*- lexical-binding: t -*-
;;; init-scala.el --- stuff related to coding in scala

(use-package scala-ts-mode
  :mode (("\.scala$" . scala-ts-mode)))

(use-package sbt-mode
  :straight
  (sbt-mode :type git :host github :repo "hvesalai/emacs-sbt-mode")
  :commands sbt-start sbt-command
  :bind (("C-c C-b" . sbt-hydra)))

;; feature-mode
(use-package feature-mode
  :config
  (defvar feature-step-search-path "src/test/scala/steps/**/*Steps.scala"))



(provide 'init-scala)
