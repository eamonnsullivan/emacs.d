;;; -*- lexical-binding: t -*-
;;; init-git.el --- stuff related to git

(require 'eds)

(use-package magit
  :bind ("<f7>" . magit-status)
  :commands (magit-status)
  :config
  (add-hook 'git-commit-setup-hook
            (lambda ()
              (eds/insert-git-branch-name (magit-get-current-branch)))))

(use-package git-gutter
  :diminish git-gutter-mode
  :init
  (setq git-gutter:deleted-sign "▁")
  (setq git-gutter:added-sign "▌")
  (setq git-gutter:modified-sign "▌")
  :config
  (global-git-gutter-mode 't))

(use-package github-browse-file
  :commands (github-browse-file)
  :bind ("C-c g h" . github-browse-file)
  :defer t)

;; (use-package github-pr-manager
;;   :config
;;   (setq debug-on-error t)
;;   :straight
;;   (github-pr-manager :type git
;;                      :host github
;;                      :branch "get-info"
;;                      :repo "eamonnsullivan/github-pr-manager"))


(provide 'init-git)
