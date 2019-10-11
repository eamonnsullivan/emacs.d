;;; -*- lexical-binding: t -*-
;;; init-git.el --- stuff related to git

(require 'eds)

(use-package magit
  :chords (" m" . magit-status)
  :commands (magit-status)
  :config
  (add-hook 'git-commit-setup-hook
            (lambda ()
              (eds/insert-git-branch-name (magit-get-current-branch)))))

(use-package git-gutter
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode 't))

(use-package github-browse-file
  :commands (github-browse-file)
  :bind ("C-c g h" . github-browse-file)
  :defer t)

(provide 'init-git)
