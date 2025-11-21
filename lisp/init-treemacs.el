;;; -*- lexical-binding: t -*-
;;; init-treemacs.el --- Stuff related to Treemacs file explorer

(use-package treemacs)

(use-package treemacs-all-the-icons
  :after treemacs
  :config
  (treemacs-load-theme "all-the-icons"))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired)

(provide 'init-treemacs)
