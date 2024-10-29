;;; -*- lexical-binding: t -*-
;;; init-copilot.el --- stuff related to Microsoft CoPilot

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :bind (("C-c C-c" . copilot-accept-completion))
  :ensure t)

(add-hook 'js2-mode-hook 'copilot-mode)
(add-hook 'scala-mode-hook 'copilot-mode)
(add-hook 'go-mode-hook 'copilot-mode)

(use-package copilot-chat
  :straight (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
  :after (request org markdown-mode))


(provide 'init-copilot)
