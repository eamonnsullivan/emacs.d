;;; -*- lexical-binding: t -*-
;;; init-scala.el --- stuff related to coding in scala

(use-package scala-mode
  :pin melpa-stable
  :defer t
  :config
  (setq scala-indent:default-run-on-strategy
        scala-indent:operator-strategy))

;; ensime
(use-package ensime
  :ensure t
  :pin melpa-stable
  :after (scala-mode)
  :bind (:map ensime-mode-map
              ("C-c m E" . ensime-reload))
  :config
  (setq ensime-search-interface 'helm)
  (setq ensime-use-helm t)
  (setq ensime-startup-notification nil)
  (setq ensime-startup-snapshot-notification nil)
  (add-hook 'scala-mode-hook #'ensime-mode)

  (use-package ensime-expand-region
    :ensure ensime
    :after (ensime)))

(use-package sbt-mode
  :pin melpa-stable
  :defer t)


;; customize ensime's implementation/test goto configuration for the
;; BBC's slightly non-standard layout in some older projects. Going
;; from the implementation to the test file works OK, but going back
;; doesn't. I still need to fix this.
(defun bbc-goto-test--is-test-dir (dir)
  (let ((case-fold-search nil))
    (or
     (string-match-p "src/test/scala/$" dir)
     (string-match-p "../test/scala/$" dir)
     (string-match-p "../tests?/$" dir))))

(defconst bbc-test-template
  "package %TESTPACKAGE%

import org.scalatest.{FlatSpec, MustMatchers}

class %TESTCLASS% extends FlatSpec with MustMatchers {

  \"%IMPLCLASS%\" should \"have a test!\" in {
    fail(\"no test\")
  }
}
"
  "The value to insert into a new test file")

(defun bbc-impl-class-name (test-class)
  (let ((suffixes (ensime-get-goto-test-config :test-class-suffixes))
        (case-fold-search nil))
    (dolist (s suffixes)
      (when (string-match-p (concat s "$") test-class)
        (message (format "Found the class name: %s for test class %s"
                         (replace-regexp-in-string (concat s "$") "" test-class)
                         test-class))
        (return (replace-regexp-in-string (concat s "$") "" test-class))))))

(setq ensime-goto-test-config-defaults
      (list :test-class-names-fn #'ensime-goto-test--test-class-names
            :test-class-suffixes '("Spec" "Test" "Check" "Specification")
            :impl-class-name-fn #'bbc-impl-class-name
            :impl-to-test-dir-fn #'ensime-goto-test--impl-to-test-dir
            :is-test-dir-fn #'bbc-goto-test--is-test-dir
            :test-template-fn (lambda () bbc-test-template)))


;; feature-mode
(use-package feature-mode
  :ensure t
  :config
  (defvar feature-step-search-path "src/test/scala/steps/**/*Steps.scala"))

;; Try to find the step defining the current feature. This isn't working yet.
(defun select-current-line ()
  (interactive)
  (move-beginning-of-line nil)
  (set-mark-command nil)
  (move-end-of-line nil)
  (setq deactivate-mark nil))

(defun get-selected-text (beg end)
  "message region or \"empty string\" if none highlighted"
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (List nil nil)))
  (message "%s" (if (and beg end)
                    (buffer-substring-no-properties beg end)
                  "empty string")))

(provide 'init-scala)
