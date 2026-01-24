;;; -*- lexical-binding: t -*-
;;; init-copilot.el --- stuff related to Microsoft CoPilot

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :bind (("C-c C-c" . copilot-accept-completion))
  :ensure t
  :hook
  ((typescript-ts-base-mode . copilot-mode)
   (scala-mode . copilot-mode)
   (go-ts-mode . copilot-mode)
   (emacs-lisp-mode . copilot-mode)
   (python-ts-mode . copilot-mode)
   (bash-ts-mode . copilot-mode)
   (java-ts-mode . copilot-mode)
   (clojure-ts-mode . copilot-mode))
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
