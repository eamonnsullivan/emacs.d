;;; -*- lexical-binding: t -*-
;;; init-git.el --- stuff related to git

(require 'eds)


(defun seq-keep (function sequence)
  "Apply FUNCTION to SEQUENCE and return the list of all the non-nil results."
  (delq nil (seq-map function sequence)))

(use-package seq)
(use-package transient)
(use-package magit
  :bind
  ("C-c g" . magit-file-dispatch)
  ("C-x g" . magit-status)
  :commands (magit-status)
  :init
  (setq magit-clone-default-directory "~/git/")
  :config
  ;; rather overly complicated method to get my hook to run only
  ;; once. Apparently, magit uses this hook in several places
  ;; internally, which causes this to run more than once sometimes.
  (setq eds-insert-branch-name-p nil) ;; initially set to nil
  (defun use-insert-branch-name (&rest args)
    ;; toggle it on. My function will run only if this is true and
    ;; immediately set it to nil.
    (setq eds-insert-branch-name-p t))
  (add-hook 'git-commit-setup-hook 'eds/insert-git-branch-name)
  (advice-add 'magit-commit :after 'use-insert-branch-name))

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

(provide 'init-git)
