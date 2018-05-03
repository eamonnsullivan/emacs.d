;;; -*- lexical-binding: t -*-
;;; init-python.el --- stuff related to python

(use-package elpy
  :ensure t
  :defer t
  :after python
  :config
  (elpy-enable))
(add-hook 'python-mode-hook 'elpy-enable)

(provide 'init-python)
