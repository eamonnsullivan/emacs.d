;;; -*- lexical-binding: t -*-
;;; init-git.el --- stuff related to git

(require 'eds)

(use-package magit
  :bind ("C-c g" . magit-file-dispatch)
  :commands (magit-status)
  :init
  (setq magit-clone-default-directory "~/git/")
  :config
  (add-hook 'git-commit-setup-hook
            (lambda ()
              (message "insert git branch called")
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
  :bind ("C-c G h" . github-browse-file)
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
