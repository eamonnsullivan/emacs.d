;;; init-git.el --- Git integration initialisation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan <me@eamonnsullivan.co.uk>

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Maintainer: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Version: 0.1
;; Created: 2018-05-03
;; Package-Requires: ((emacs "25.1") (magit "3.0"))
;; Keywords: git, vc, tools, version-control
;; URL: https://github.com/eamonnsullivan/init-git

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides initialisation and configuration for Git integration
;; in Emacs, enabling advanced version control features and workflow.

;;; Licence:

;; This programme is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public Licence as published by
;; the Free Software Foundation, either version 3 of the Licence, or
;; (at your option) any later version.

;; This programme is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public Licence for more details.

;; You should have received a copy of the GNU General Public Licence
;; along with this programme.  If not, see <https://www.gnu.org/licenses/>.

(require 'eds-utils)

(use-package seq)
(use-package transient)
(use-package magit
  :bind
  ("C-c g" . magit-file-dispatch)
  ("C-x g" . magit-status)
  :commands (magit-status)
  :init
  (setopt magit-clone-default-directory "~/git/")
  :config
  ;; rather overly complicated method to get my hook to run only
  ;; once. Apparently, magit uses this hook in several places
  ;; internally, which causes this to run more than once sometimes.
  (setopt eds-insert-branch-name-p nil) ;; initially set to nil
  (defun use-insert-branch-name (&rest args)
    ;; toggle it on. My function will run only if this is true and
    ;; immediately set it to nil.
    (setopt eds-insert-branch-name-p t))
  (add-hook 'git-commit-setup-hook 'eds-utils/insert-git-branch-name)
  (advice-add 'magit-commit :after 'use-insert-branch-name))

(use-package git-gutter
  :diminish git-gutter-mode
  :init
  (setopt git-gutter:deleted-sign "▁")
  (setopt git-gutter:added-sign "▌")
  (setopt git-gutter:modified-sign "▌")
  :config
  (global-git-gutter-mode 't))

(use-package github-browse-file
  :commands (github-browse-file)
  :bind ("C-c G h" . github-browse-file)
  :defer t)

(provide 'init-git)
;;; init-git.el ends here
