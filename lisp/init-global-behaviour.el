;;; -*- lexical-binding: t -*-
;;; init-global-behavior.el --- Things I always want, no matter the mode

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
  :config
  (global-font-lock-mode)
  (size-indication-mode)
  (column-number-mode)
  (minibuffer-electric-default-mode)
  (show-paren-mode)
  (electric-pair-mode)
  (global-subword-mode)
  (delete-selection-mode)
  :init
  (put 'narrow-to-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  :custom
  (show-paren-delay 0)
  (show-paren-style 'mixed)
  (font-lock-maximum-decoration t)
  (inhibit-startup-screen t "Don't show splash screen")
  (inhibit-startup-message t "Don't show the message, either")
  (use-dialog-box nil "Disabled non-accessible dialog boxes")
  (indent-tabs-mode nil "Use spaces, always")
  (delete-by-moving-to-trash t "Move to trash folder")
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
  (mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (mouse-wheel-progressive-speed nil)
  (mouse-wheel-follow-mouse t)
  (ring-bell-function 'ignore)
  (track-eol t)
  (line-move-visual nil)
  (visible-bell)
  (warning-suppress-log-types '((comp)))
  (package-install-upgrade-built-in t)
  (calendar-date-style 'european)
  (tramp-default-method "ssh")
  (remote-file-name-inhibit-locks t)
  (tramp-use-scp-direct-remote-copying t)
  (remote-file-name-inhibit-auto-save-visited t)
  (tramp-copy-size-limit (* 1024 1024))
  (tramp-verbose 2)
  (find-file-visit-truename t)
  :hook
  ((text-mode . visual-line-mode)
   (before-save . delete-trailing-whitespace)))

(setq package-install-upgrade-built-in t)

;; Turn off the annoying default backup behaviour
(let ((backup-dir (concat (file-name-directory user-init-file) "backup")))
  (if (file-directory-p backup-dir)
      (setq backup-directory-alist `((".*" . ,backup-dir))
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
  (setq key-chord-two-keys-delay 0.05)
  :config
  (key-chord-mode 1)
  (require 'eds)
  (key-chord-define-global "JJ" 'eds/switch-to-previous-buffer))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(use-package hippie-exp                 ; Powerful expansion and completion
  :bind (([remap dabbrev-expand] . hippie-expand))
  :config
  (progn
    (setq hippie-expand-try-functions-list
          '(try-expand-dabbrev
            try-expand-dabbrev-all-buffers
            try-expand-dabbrev-from-kill
            try-complete-file-name-partially
            try-complete-file-name
            try-expand-all-abbrevs
            try-expand-list
            try-complete-lisp-symbol-partially
            try-complete-lisp-symbol))))

(use-package autorevert ; Auto-revert buffers of changed files
  :init (global-auto-revert-mode)
  :config
  (setq auto-revert-verbose nil
        global-auto-revert-non-file-buffers t)

  (when (eq system-type 'darwin)
    ;; File notifications aren't supported on OS X
    (setq auto-revert-use-notify nil))
  :diminish (auto-revert-mode . " Ⓐ"))

;; Display available keybindings in popup
(use-package which-key
  :diminish which-key-mode
  :bind (:map help-map ("C-h" . which-key-C-h-dispatch))
  :init (add-hook 'after-init-hook #'which-key-mode))

(use-package discover-my-major
  :bind (("C-h M-m" . discover-my-major)
         ("C-h M-M" . discover-my-mode)))

;; font scaling
(use-package default-text-scale)

(use-package dashboard
  :init
  (dashboard-setup-startup-hook)
  :config
  (setq dashboard-projects-backend 'projectile
        dashboard-items '((recents   . 5)
                          (projects  . 5)
                          (agenda    . 5)))
  (require 'eds)
  (add-to-list 'dashboard-item-generators  '(custom . eds/dashboard-custom-conflicted-files))
  (add-to-list 'dashboard-items '(custom) t))


(use-package eldoc-eval
  :config
  (eldoc-in-minibuffer-mode 1))

(use-package eldoc
  :config
  (setq eldoc-echo-area-use-multiline-p t))

(use-package pomidor
  :bind (("<f12>" . pomidor))
  :config (setq pomidor-sound-tick nil
                pomidor-sound-tack nil
                pomidor-sound-overwork (expand-file-name (concat pomidor-dir "overwork.wav")))
  :hook (pomidor-mode . (lambda ()
                          (display-line-numbers-mode -1) ; Emacs 26.1+
                          (setq left-fringe-width 0 right-fringe-width 0)
                          (setq left-margin-width 2 right-margin-width 0)
                          ;; force fringe update
                          (set-window-buffer nil (current-buffer)))))

(use-package super-save
  :defer 1
  :diminish super-save-mode
  :config
  (super-save-mode +1)
  (setq super-save-exclude '("\\*dashboard\\*"))
  (setq super-save-auto-save-when-idle t))

(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))

(provide 'init-global-behaviour)
