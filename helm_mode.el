;;; -*- lexical-binding: t -*-
;;; helm_mode.el --- stuff related to helm-mode

;; Copyright (c) 2017 Eamonn Sullivan

;; Author: Eamonn Sullivan <eamonn.sullivan@gmail.com>
;; Maintainer: Eamonn Sullivan <eamonn.sullivan@gmail.com>
;; Created 23 March 2017

;; Homepage: https://github.com/eamonnsullivan/emacs.d

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;;; Code:

;; helm
(use-package helm
  :ensure t
  :diminish helm-mode
  :config
  (require 'helm-config)
  (defvar helm-google-suggest-use-curl-p)
  (defvar helm-ff-search-library-in-sexp)
  (defvar helm-ff-file-name-history-use-recentf)
  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "M-x") 'undefined)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-h i") 'helm-info)
  (global-set-key (kbd "<f1>") 'helm-resume)
  (global-set-key (kbd "C-s")  'helm-occur)
  (define-key global-map (kbd "M-g a") 'helm-do-grep-ag)
  (define-key global-map (kbd "M-g g") 'helm-grep-do-git-grep)

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  (setq helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                    4 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t
        helm-echo-input-in-header-line        t
        helm-candidate-separator              "––––––––––––––––––––––––––––––––––––––"
        helm-net-prefer-curl                  nil
        helm-kill-ring-threshold              1
        helm-input-idle-delay                 0.01
        helm-ff-auto-update-initial-value     t
        helm-grep-ag-command                  "ag --color=always --smart-case --no-heading --line-number %s %s %s"
        helm-reuse-last-window-split-state    t
        helm-always-two-windows               t
        helm-split-window-inside-p            t
        helm-show-action-window-other-window  'left
        helm-buffers-favorite-modes           '(append helm-buffers-favorite-modes '(picture-mode artist-mode))
        helm-ls-git-status-command            'magit-status-internal
        helm-M-x-requires-pattern             0
        helm-boring-file-regexp-list          '("\\.git/\\|\\.git$" "\\.hg/\\|\\.hg$" "\\.svn/\\|\\.svn$"
                                                "\\.CVS/\\|\\.CVS$" "\\._darcs/\\|\\._darcs$" "\\.la$"
                                                "\\.o$" "\\.i$" "\\.steam/\\|\\.steam$"
                                                "undo-tree-history/\\|undo-tree-history$"
                                                "\\.Private/\\|\\.Private$" "\\.encrypted/\\|\\.encrypted$"
                                                "emacs_backup/\\|emacs_backup$")
        helm-buffer-skip-remote-checking      t
        helm-allow-mouse                      t
        helm-apropos-fuzzy-match              t
        helm-M-x-fuzzy-match                  t
        helm-lisp-fuzzy-completion            t
        helm-completion-in-region-fuzzy-match t
        helm-buffers-fuzzy-matching           t
        helm-locate-fuzzy-match               t
        helm-move-to-line-cycle-in-source     t
        helm-tramp-verbose                    6
        helm-org-headings-fontify             t
        helm-autoresize-max-height            50 ; it is %.
        helm-autoresize-min-height            20 ; it is %.
        fit-window-to-buffer-horizontally     1
        helm-open-github-closed-issue-since   720 ; 2 years
        helm-turn-on-recentf                  nil
        helm-mini-default-sources             '(helm-source-buffers-list helm-source-buffer-not-found)
        helm-follow-mode-persistent           t
        helm-browse-project-default-find-files-fn 'helm-browse-project-ag-find-files
        helm-emms-use-track-description-function nil)

  (defun helm-hide-minibuffer-maybe ()
    "Hide minibuffer in Helm session if we use the header line as input field."
    (when (with-helm-buffer helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'face
                     (let ((bg-color (face-background 'default nil)))
                       `(:background ,bg-color :foreground ,bg-color)))
        (setq-local cursor-type nil))))


  (if (and (boundp 'tab-always-indent)
           (eq tab-always-indent 'complete)
           (boundp 'completion-in-region-function))
      (progn
        (define-key lisp-interaction-mode-map [remap indent-for-tab-command] 'helm-multi-lisp-complete-at-point)
        (define-key emacs-lisp-mode-map       [remap indent-for-tab-command] 'helm-multi-lisp-complete-at-point)

        ;; lisp complete. (Rebind M-<tab>)
        (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
        (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))

    (define-key lisp-interaction-mode-map [remap indent-for-tab-command] 'helm-multi-lisp-complete-at-point)
    (define-key emacs-lisp-mode-map       [remap indent-for-tab-command] 'helm-multi-lisp-complete-at-point)

    ;; lisp complete. (Rebind M-<tab>)
    (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
    (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))

  (unless (boundp 'completion-in-region-function)
    (add-hook 'ielm-mode-hook
              #'(lambda ()
                  (define-key ielm-map [remap completion-at-point] 'helm-lisp-completion-at-point))))

  ;; Cycle resume
  (helm-define-key-with-subkeys global-map (kbd "C-c n") ?n 'helm-cycle-resume)

  ;; sh-mode
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-info-bash)

  (add-hook 'helm-minibuffer-set-up-hook
            'helm-hide-minibuffer-maybe)

  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 40)

  (helm-autoresize-mode 1)

  (helm-mode 1))

;; helm-ag stuff
(use-package helm-ag
  :ensure t
  :bind (("\C-c r" . helm-do-grep-ag))
  :init
  (setq helm-ag-use-grep-ignore-list t)
  (setq helm-ag-use-agignore t)
  (setq helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
  (setq helm-ag-command-option "--all-text")
  (setq helm-ag-ignore-buffer-patterns '("\\.txt\\'" "\\.mkd\\'" "\\.class\\'"))
  (setq helm-ag-insert-at-point 'symbol))

;;; helm_mode.el ends here
