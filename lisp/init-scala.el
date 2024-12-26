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

(straight-use-package '(scala-ts-mode :type git :host github
                                      :repo "KaranAhlawat/scala-ts-mode"))

;; (use-package scala-mode
;;   :interpreter ("scala" . scala-mode)
;;   :config
;;   (setq scala-indent:default-run-on-strategy
;;         scala-indent:operator-strategy))

(use-package sbt-mode
  :straight
  (sbt-mode :type git :host github :repo "hvesalai/emacs-sbt-mode")
  :commands sbt-start sbt-command
  :bind (("C-c C-b" . sbt-hydra)
         ("C-c t" . annotate-scala-symbol-with-type))
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  (setq sbt:program-options '("-Dsbt.supershell=false")))

;; feature-mode
(use-package feature-mode
  :config
  (defvar feature-step-search-path "src/test/scala/steps/**/*Steps.scala"))



(provide 'init-scala)
