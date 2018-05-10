;;; -*- lexical-binding: t -*-
;;; init-python.el --- stuff related to python

(use-package elpy
  :defer t
  :after python
  :hook ((python-mode . elpy-enable))
  :config
  (elpy-enable))

(provide 'init-python)
