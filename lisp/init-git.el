;;; -*- lexical-binding: t -*-
;;; init-git.el --- stuff related to git

(use-package magit
  :ensure t
  :chords (" m" . magit-status)
  :commands (magit-status))

(use-package git-timemachine
  :ensure t
  :bind (("C-z g" . git-timemachine)))

(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode 't))

(use-package github-browse-file
  :commands (github-browse-file)
  :bind ("C-c g h" . github-browse-file)
  :defer t
  :ensure t)

(provide 'init-git)
