;;; -*- lexical-binding: t -*-
;;; init-scala.el --- stuff related to coding in scala

(require 'eds)

(defun is-assignment-thing-p (line)
  "Returns t if a line appears to be a def, val or var assignment. Nil otherwise."
  (and (string-match-p "=" line) (or (string-match-p "def " line)
                                     (string-match-p "val " line)
                                     (string-match-p "var " line))))

(defun has-annotation-p (line)
  "Returns t if the line appears to already have a type annotation. Nil otherwise."
  (string-match-p "\:[^)]+\=" line))

(use-package scala-mode
  :interpreter ("scala" . scala-mode))

(use-package sbt-mode
  :straight
  (sbt-mode :type git :host github :repo "hvesalai/emacs-sbt-mode")
  :commands sbt-start sbt-command
  :bind (("C-c C-b" . sbt-hydra))
  :config
  ;; WORKAROUND allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

;; feature-mode
(use-package feature-mode
  :config
  (defvar feature-step-search-path "src/test/scala/steps/**/*Steps.scala"))



(provide 'init-scala)
