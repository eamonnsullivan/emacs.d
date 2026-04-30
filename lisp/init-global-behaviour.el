;;; init-global-behaviour.el --- Global behaviour initialisation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan <me@eamonnsullivan.co.uk>

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Maintainer: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Created: 2017-01-02
;; Version: 0.1
;; Package-Requires: ((emacs "31.0"))
;; Keywords: behaviour, convenience, editing, workflow
;; URL: https://github.com/eamonnsullivan/init-global-behaviour

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides initialisation and configuration for global behaviour
;; in Emacs, supporting improved editing, convenience, and workflow.

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

(use-package crux
  :bind (("C-k"                          . crux-smart-kill-line)
         ([(control shift return)]       . crux-smart-open-line-above)
         ([(shift return)]               . crux-smart-open-line)
         ([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ("s-r"                          . crux-recentf-find-file)
         ("C-c D"                        . crux-delete-file-and-buffer)
         ("C-c r"                        . crux-rename-file-and-buffer)
         ("C-c k"                        . crux-kill-other-buffers))
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-point-to-eol kill-ring-save)
  (defalias 'rename-file-and-buffer #'crux-rename-file-and-buffer))

(use-package emacs
  :bind  ; NOTE: M-x describe-personal-bindings (for all use-packge binds)
  (("M-o" . other-window)
   ("M-j" . duplicate-dwim)
   ("M-s g" . grep)
   ("C-x ;" . comment-line)
   ("C-x w t"  . window-layout-transpose)            ; EMACS-31
   ("C-x w r"  . window-layout-rotate-clockwise)     ; EMACS-31
   ("C-x w f h"  . window-layout-flip-leftright)     ; EMACS-31
   ("C-x w f v"  . window-layout-flip-topdown)       ; EMACS-31
   ("C-x 5 l"  . select-frame-by-name)
   ("C-x 5 s"  . set-frame-name)
   ("RET" . newline-and-indent)
   ("C-z" . nil)
   ("C-M-z" . delete-pair))
  :config
  (global-font-lock-mode)
  (size-indication-mode)
  (column-number-mode)
  (minibuffer-electric-default-mode)
  (show-paren-mode)
  (electric-pair-mode)
  (global-subword-mode)
  (delete-selection-mode)
  (setopt savehist-additional-variables
          '(search-ring regexp-search-ring kill-ring))
  (require 'info)
  (info-initialize)
  (push "/opt/homebrew/share/info/" Info-directory-list)
  (add-to-list 'Info-default-directory-list "/opt/homebrew/share/info")
  (setq-default visual-fill-column-width 120)
  :init
  (put 'narrow-to-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  :custom
  (recentf-max-saved-items 300) ; default is 20
  (recentf-max-menu-items 15)
  (recentf-auto-cleanup (if (daemonp) 300 'never))
  (recentf-exclude (list "^/\\(?:ssh\\|su\\|sudo\\)?:"))
  (recentf-save-file (expand-file-name "cache/recentf" user-emacs-directory))
  (treesit-auto-install-grammar t) ; EMACS-31
  (treesit-enabled-modes t)        ; EMACS-31
  (calendar-latitude 51.5)
  (calendar-longitude -0.12)
  (calendar-location-name "London, England")
  (show-paren-delay 0)
  (show-paren-style 'mixed)
  (font-lock-maximum-decoration t)
  (elisp-fontify-semantically t)
  (inhibit-startup-screen t "Don't show splash screen")
  (inhibit-startup-message t "Don't show the message, either")
  (use-dialog-box nil "Disabled non-accessible dialog boxes")
  (indent-tabs-mode nil "Use spaces, always")
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (delete-by-moving-to-trash t "Move to trash folder")
  (dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso")
  (dired-dwim-target t)
  (confirm-kill-processes nil "Don't require confirmation to kill background processes")
  (require-final-newline t "Also make sure there's a newline at the end")
  (starttls-use-gnutls t)
  (gnutls-log-level 0)
  (echo-keystrokes 0.02)
  (indicate-buffer-boundaries 'left)
  (prettify-symbols-unprettify-at-point 'right-edge)
  (column-number-indicator-zero-based nil)
  (save-interprogram-paste-before-kill t)
  (make-pointer-invisible t)
  (mouse-drag-copy-region t)
  (mouse-wheel-progressive-speed nil)
  (mouse-wheel-follow-mouse t)
  (ring-bell-function 'ignore)
  (track-eol t)
  (line-move-visual nil)
  (visible-bell)
  (warning-suppress-log-types '((comp)))
  (package-install-upgrade-built-in t)
  (calendar-date-style 'european)
  (remote-file-name-inhibit-locks t)
  (remote-file-name-inhibit-auto-save-visited t)
  (find-file-visit-truename t)
  (auth-source "~/.authinfo.gpg")
  (mode-line-collapse-minor-modes '(auto-fill-mode flyspell-mode eldoc-mode abbrev-mode copilot-mode yasnippet-mode))
  (use-short-answers t)
  (isearch-lazy-count t)
  (auto-save-default nil)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (save-interprogram-paste-before-kill t)
  (redisplay-skip-fontification-on-input t)
  (kill-do-not-save-duplicates t)
  (window-combination-resize t)
  (world-clock-time-format "%z %R	%a %d %b (%Z)")
  (reb-re-syntax 'string)
  (help-window-select t)
  (repeat-mode t)
  (context-menu-mode t)
  (enable-recursive-minibuffers t)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  :hook
  ((text-mode . (lambda ()
                  (visual-line-mode 1)
                  (visual-fill-column-mode 1)
                  (visual-fill-column-toggle-center-text)))
   (before-save . delete-trailing-whitespace)))

(require 'init-mu4e)
(set-variable 'mail-user-agent 'mu4e-user-agent)
(set-variable 'read-mail-command 'mu4e)
(setopt package-install-upgrade-built-in t)

;; Turn off the annoying default backup behaviour
(let ((backup-dir (concat (file-name-directory user-init-file) "backup")))
  (if (file-directory-p backup-dir)
      (setopt backup-directory-alist `((".*" . ,backup-dir))
              auto-save-file-name-transforms `((".*" ,backup-dir t))
              backup-by-copying t         ; Don't delink hardlinks
              version-control t           ; Use version numbers on backups
              delete-old-versions t       ; Automatically delete excess backups
              kept-new-versions 20        ; how many of the newest versions to keep
              kept-old-versions 5         ; and how many of the old
              delete-by-moving-to-trash t
              create-lockfiles nil        ; don't create lockfiles
              view-read-only t            ; use view mode on read-only buffers.
              sentence-end-double-space nil
              )
    (message (format "Directory does not exist: %s" backup-dir))))

;; from http://www.coli.uni-saarland.de/~slemaguer/emacs/main.html
(use-package flyspell
  :config

  ;; Set programms
  (add-hook 'text-mode-hook 'flyspell-mode)
  (setq-default ispell-program-name "aspell")
  (setq-default ispell-list-command "--list")

  ;; Refresh flyspell after directory change
  (defun flyspell-buffer-after-pdict-save (&rest _)
    (flyspell-buffer))
  (advice-add 'ispell-pdict-save :after #'flyspell-buffer-after-pdict-save)

  ;; Popup
  (defun flyspell-emacs-popup-textual (event poss word)
    "A textual flyspell popup menu."
    (require 'popup)
    (let* ((corrects (if flyspell-sort-corrections
                         (sort (car (cdr (cdr poss))) 'string<)
                       (car (cdr (cdr poss)))))
           (cor-menu (if (consp corrects)
                         (mapcar (lambda (correct)
                                   (list correct correct))
                                 corrects)
                       '()))
           (affix (car (cdr (cdr (cdr poss)))))
           show-affix-info
           (base-menu  (let ((save (if (and (consp affix) show-affix-info)
                                       (list
                                        (list (concat "Save affix: " (car affix))
                                              'save)
                                        '("Accept (session)" session)
                                        '("Accept (buffer)" buffer))
                                     '(("Save word" save)
                                       ("Accept (session)" session)
                                       ("Accept (buffer)" buffer)))))
                         (if (consp cor-menu)
                             (append cor-menu (cons "" save))
                           save)))
           (menu (mapcar
                  (lambda (arg) (if (consp arg) (car arg) arg))
                  base-menu)))
      (cadr (assoc (popup-menu* menu :scroll-bar t) base-menu))))


  (defun flyspell-emacs-popup-choose (org-fun event poss word)
    (if (window-system)
        (funcall org-fun event poss word)
      (flyspell-emacs-popup-textual event poss word)))

  (eval-after-load "flyspell"
    '(progn
       (advice-add 'flyspell-emacs-popup :around #'flyspell-emacs-popup-choose))))

(use-package use-package-chords
  :init
  (setopt key-chord-two-keys-delay 0.05)
  :config
  (key-chord-mode 1)
  (key-chord-define-global "JJ" 'eds-utils/switch-to-previous-buffer))

(require 'uniquify)
(setopt uniquify-buffer-name-style 'forward)

(use-package autorevert ; Auto-revert buffers of changed files
  :init (global-auto-revert-mode)
  :config
  (setopt auto-revert-verbose nil
          global-auto-revert-non-file-buffers t)

  (when (eq system-type 'darwin)
    ;; File notifications aren't supported on OS X
    (setopt auto-revert-use-notify nil))
  :diminish (auto-revert-mode . " Ⓐ"))

;; Display available keybindings in popup
(use-package which-key
  :diminish which-key-mode
  :bind (:map help-map ("C-h" . which-key-C-h-dispatch))
  :init (add-hook 'after-init-hook #'which-key-mode))

(use-package which-key-posframe
  :config
  (which-key-posframe-mode 1))

(use-package discover-my-major
  :bind (("C-h M-m" . discover-my-major)
         ("C-h M-M" . discover-my-mode)))

;; font scaling
(use-package default-text-scale)

(use-package dashboard
  :init
  (dashboard-setup-startup-hook)
  :config
  (setopt dashboard-projects-backend 'projectile
          dashboard-items '((recents   . 5)
                            (projects  . 5)
                            (agenda    . 5)))
  (add-to-list 'dashboard-item-generators  '(custom . eds-utils/dashboard-custom-conflicted-files))
  (add-to-list 'dashboard-items '(custom) t))


(use-package eldoc-eval
  :config
  (eldoc-in-minibuffer-mode 1))

(use-package eldoc
  :config
  (setopt eldoc-echo-area-use-multiline-p t))

(use-package pomidor
  :bind (("<f12>" . pomidor))
  :config (setopt pomidor-sound-tick nil
                  pomidor-sound-tack nil
                  pomidor-sound-overwork (expand-file-name (concat pomidor-dir "overwork.wav")))
  :hook (pomidor-mode . (lambda ()
                          (display-line-numbers-mode -1) ; Emacs 26.1+
                          (setopt left-fringe-width 0 right-fringe-width 0)
                          (setopt left-margin-width 2 right-margin-width 0)
                          ;; force fringe update
                          (set-window-buffer nil (current-buffer)))))

(use-package super-save
  :straight t
  :config
  (super-save-mode +1)
  :custom
  (super-save-exclude '("\\*dashboard\\*"))
  (super-save-auto-save-when-idle t))

(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))

(use-package unfill)

(defvar fill-paragraph-state nil
  "The way the paragraph was filled the last time.")

(defun eds/fill-paragraph-toggle ()
  "Fill and unfill the current paragraph, depending on what was done last time."
  (interactive)
  (unless (eq last-command this-command)
    (setopt fill-paragraph-state nil))
  (let (deactivate-mark)
    (cl-case fill-paragraph-state
      ('fill-paragraph
       (call-interactively 'unfill-paragraph)
       (setopt fill-paragraph-state 'unfill-paragraph))
      (t
       (call-interactively 'fill-paragraph)
       (setopt fill-paragraph-state 'fill-paragraph)))))

(global-unset-key (kbd "M-q"))
(global-set-key (kbd "M-q") 'eds/fill-paragraph-toggle)

(setopt world-clock-list
        '(("Europe/London" "London")
          ("America/New_York" "Boston")
          ("UTC" "UTC")
          ("Europe/Paris" "Berlin")
          ("America/Los_Angeles" "San Francisco")
          ("Asia/Tokyo" "Tokyo")
          ("Australia/Sydney" "Sydney")))

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; enable smooth pixel scrolling on graphical displays
(pixel-scroll-precision-mode t)

(provide 'init-global-behaviour)
;;; init-global-behaviour.el ends here
