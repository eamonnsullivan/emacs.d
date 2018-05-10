;;; -*- lexical-binding: t -*-
;;; init-git.el --- stuff related to git

(use-package magit
  :chords (" m" . magit-status)
  :commands (magit-status))

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
