;;; -*- lexical-binding: t -*-
;;; init-projectile.el --- stuff related to projectile project management

(require 'helm)

(use-package projectile
  :demand
  :diminish projectile-mode
  :init
  (setq projectile-use-git-grep t
        projectile-enable-caching t
        projectile-completion-system 'helm
        projectile-switch-project-action 'projectile-find-file)
  (use-package helm-projectile
    :after projectile
    :config
    (helm-projectile-on)
    :bind (("C-c p p" . helm-projectile-switch-project)))
  :config
  (projectile-mode t)
  (add-to-list 'projectile-globally-ignored-directories "node-modules")
  (add-to-list 'projectile-globally-ignored-files "node-modules")
  :bind   (("s-f" . helm-projectile-find-file)
           ("s-F" . helm-projectile-grep)))

(provide 'init-projectile)
