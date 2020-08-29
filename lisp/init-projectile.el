;;; -*- lexical-binding: t -*-
;;; init-projectile.el --- stuff related to projectile project management

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
  :bind   (("s-f" . projectile-find-file)
           ("s-F" . projectile-grep)))

(provide 'init-projectile)
