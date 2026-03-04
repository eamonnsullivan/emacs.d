;;; package -- init-copilot.el -*- lexical-binding: t -*-
;;; Copyright (c) 2026 Eamonn Sullivan
;;; Commentary:
;;; Microsoft CoPilot-related packages
;;;
;;; Code:


(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t
  :hook
  (prog-mode . copilot-mode)
  (prog-mode . copilot-nes-mode)
  :bind (:map copilot-completion-map
              ("C-c C-c" . copilot-accept-completion)
              ("C-c C-c" . copilot-accept-completion)
              ("C-n" . copilot-next-completion)
              ("C-p" . copilot-previous-completion))
  :config
  ;; Remap for copilot, since I don't actually use this key in python
  ;; much.
  (unbind-key "C-c C-c" python-ts-mode-map)
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-to-list 'copilot-indentation-alist '(closure-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2)))

(use-package copilot-chat
  :straight (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
  :config
  (global-unset-key (kbd "C-c i"))
  :bind (("C-c i" . copilot-chat-goto-input))
  :after (request org markdown-mode))

;; (add-hook 'git-commit-setup-hook 'copilot-chat-insert-commit-message)


(provide 'init-copilot)
