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
  :bind   (("s-F" . helm-projectile-grep)
           ("C-c p h" . helm-projectile-find-file)))

(provide 'init-projectile)
