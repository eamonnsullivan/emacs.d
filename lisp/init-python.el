;;; -*- lexical-binding: t -*-
;;; init-python.el --- stuff related to python

(use-package elpy
  :defer t
  :after python
  :hook ((python-mode . elpy-enable))
  :config
  (setq elpy-rpc-python-command "/home/linuxbrew/.linuxbrew/bin/python3")
  (elpy-enable))

(provide 'init-python)
