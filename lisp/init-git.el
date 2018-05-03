;;; -*- lexical-binding: t -*-
;;; init-git.el --- stuff related to git

(use-package magit
  :ensure t)

(use-package git-timemachine
  :ensure t
  :bind (("C-z g" . git-timemachine)))

(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode 't))

(provide 'init-git)
