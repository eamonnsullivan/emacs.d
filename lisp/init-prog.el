;;; init-prog.el --- Programming mode initialisation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan <me@eamonnsullivan.co.uk>

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Maintainer: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Created: 2018-05-03
;; Version: 0.1
;; Package-Requires: ((emacs "31.0"))
;; Keywords: programming, languages, tools
;; URL: https://github.com/eamonnsullivan/init-prog

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides initialisation and configuration for programming modes,
;; supporting editing, syntax highlighting, and workflow enhancements for various languages in Emacs.

;;; Licence:

;; This programme is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public Licence as published by
;; the Free Software Foundation, either version 3 of the Licence, or
;; (at your option) any later version.

;; This programme is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public Licence for more details.

;; You should have received a copy of the GNU General Public Licence
;; along with this programme.  If not, see <https://www.gnu.org/licenses/>.

(defvar imenu-auto-rescan t)


;; comint
(require 'comint)
(setopt ansi-color-for-comint-mode t)
(defun eds/init-comint ()
  ;; Don't jump around when output in a buffer happens
  (set (make-local-variable 'scroll-conservatively) 1000))
(add-hook 'comint-mode-hook 'eds/init-comint)

(use-package idle-highlight-mode
  :config (setopt idle-highlight-idle-time 0.2))

(use-package typescript-ts-mode
  :mode (("\.ts$" . typescript-ts-mode))
  :config (add-hook 'typescript-ts-base-mode-hook
                    (lambda()
                      (setopt typescript-indent-level 2))))

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
  (setopt plantuml-jar-path "/usr/local/bin/plantuml"
        plantuml-default-exec-mode 'executable))

(use-package yaml-mode
  :mode ("\\.yml\\'" . yaml-mode)
  :hook
  (yaml-mode . (lambda () (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(add-to-list 'auto-mode-alist '("\\.service\\'" . conf-unix-mode))

(use-package graphql-mode)

(use-package jupyter
  :straight t)

(straight-use-package 'yapfify)
(add-hook 'python-ts-mode-hook 'yapf-mode)


(require 'treesit)

(use-package haskell-mode)

(use-package treesit-auto
  :straight (treesit-auto
             :type git
             :host github
             :repo "renzmann/treesit-auto"
             :fork ( :host github
                     :repo "noctuid/treesit-auto"
                     :branch "bind-around-set-auto-mode-0")))

(setopt treesit-font-lock-level 4)

(use-package rainbow-mode
  :straight t
  :defer t
  :config
  (dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook el-mode-hook js-mode-hook ts-mode-hook))
                (add-hook hook 'rainbow-mode)))

(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)

(provide 'init-prog)
;;; init-prog.el ends here
