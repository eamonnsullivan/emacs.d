;;; -*- lexical-binding: t -*-
;;; init-projectile.el --- stuff related to projectile project management

(use-package projectile
  :demand
  :diminish projectile-mode
  :init
  (setq projectile-use-git-grep t)
  :config
  (projectile-global-mode t)
  (setq projectile-completion-system 'helm)
  (setq projectile-switch-project-action 'projectile-find-file)
  (add-to-list 'projectile-globally-ignored-directories "node-modules")
  (add-to-list 'projectile-globally-ignored-files "node-modules")
  :bind   (("s-f" . projectile-find-file)
           ("s-F" . projectile-grep)))

(provide 'init-projectile)
