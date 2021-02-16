;;; -*- lexical-binding: t -*-
;;; init-scala.el --- stuff related to coding in scala

(require 'lsp-mode)
(require 'eds)

(defun is-assignment-thing-p (line)
  "Returns t if a line appears to be a def, val or var assignment. Nil otherwise."
  (and (string-match-p "=" line) (or (string-match-p "def " line)
                                     (string-match-p "val " line)
                                     (string-match-p "var " line))))

(defun has-annotation-p (line)
  "Returns t if the line appears to already have a type annotation. Nil otherwise."
  (string-match-p "\:[^)]+\=" line))

(defun annotate-scala-symbol-with-type ()
  "Using lsp, if available, append the type of the scala symbol (def, val or var) at point"
  (interactive)
  (when (equal (lsp-buffer-language) "scala")
    (let* ((sym-type (eds/get-symbol-and-type-of-thing-at-point))
          (sym (car sym-type))
          (type (car (cdr sym-type)))
          (line (string-trim (thing-at-point 'line t))))
      (if (and  (is-assignment-thing-p line) (not (has-annotation-p line)))
          (let ((sym-with-type (format "%s: %s" sym type)))
            (save-excursion
              (save-restriction
                (let ((start (line-beginning-position))
                      (end (line-end-position)))
                  (narrow-to-region start end)
                  (goto-char (point-min))
                  (while (search-forward sym nil t)
                    (replace-match sym-with-type))))))))))

(use-package scala-mode
  :config
  (setq scala-indent:default-run-on-strategy
        scala-indent:operator-strategy))

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
