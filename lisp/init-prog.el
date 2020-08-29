;;; -*- lexical-binding: t -*-
;;; init-prog.el --- stuff related to programming, in general

(defvar imenu-auto-rescan t)

(add-to-list 'auto-mode-alist '("\\.service\\'" . conf-unix-mode))

;; comint
(require 'comint)
(setq ansi-color-for-comint-mode t)
(defun eds/init-comint ()
  ;; Don't jump around when output in a buffer happens
  (set (make-local-variable 'scroll-conservatively) 1000))
(add-hook 'comint-mode-hook 'eds/init-comint)

(use-package highlight-symbol
  :bind (("M-n" . highlight-symbol-next)
         ("M-p" . highlight-symbol-prev))
  :chords (("XX" . highlight-symbol)))

(use-package groovy-mode
  :mode (("\.groovy$" . groovy-mode))
  :hook
  (groovy-mode . (lambda() (inf-groovy-keys)))
  :config
  (setq groovysh "/usr/local/bin/groovysh")
  (autoload 'run-groovy "inf-groovy" "Run an inferior Groovy process")
  (autoload 'inf-groovy-keys "inf-groovy" "Set local key defs for inf-groovy in groovy-mode"))
(use-package groovy-imports)

;; enable colour in compile and sbt modes (this doesn't work for cucumber)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'sbt-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

;; Highlight Comment Annotations
;; from: https://gitlab.com/psachin/emacs.d/blob/master/custom_functions.org
(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.
This functions should be added to the hooks of major modes for
programming."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))
(add-hook 'prog-mode-hook 'font-lock-comment-annotations)

(use-package plantuml-mode
  :mode (("\\.puml\\'" . plantuml-mode)
         ("\\.plantuml\\'" . plantuml-mode))
  :config
  (setq plantuml-jar-path "/usr/local/bin/plantuml"
        plantuml-default-exec-mode 'executable))

(use-package yaml-mode
  :mode ("\\.yml\\'" . yaml-mode)
  :hook
  (yaml-mode . (lambda () (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(provide 'init-prog)
