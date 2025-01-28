;;; -*- lexical-binding: t -*-
;;; init-copilot.el --- stuff related to Microsoft CoPilot

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :bind (("C-c C-c" . copilot-accept-completion))
  :ensure t
  :config
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-to-list 'copilot-indentation-alist '(closure-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2)))

(add-hook 'typescript-ts-base-mode-hook 'copilot-mode)
(add-hook 'scala-ts-mode-hook 'copilot-mode)
(add-hook 'go-ts-mode-hook 'copilot-mode)

(use-package copilot-chat
  :straight (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
  :after (request org markdown-mode))


(provide 'init-copilot)
