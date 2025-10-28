;;; -*- lexical-binding: t -*-
;;; init-eca.el --- stuff related to ECA

(use-package eca
  :straight (:host github :repo "editor-code-assistant/eca-emacs" :files ("*.el"))
  ;; :bind (("C-c C-c" . eca-complete))
  :hook
  ((typescript-ts-base-mode . eca-completion-mode)
   (scala-ts-mode . eca-completion-mode)
   (emacs-lisp-mode . eca-completion-mode)
   (python-ts-mode . eca-completion-mode)
   (bash-ts-mode . eca-compltion-mode)))

(provide 'init-eca)
