;;; early-init.el -- early initialization code for emacs  -*- lexical-binding: t; -*-

;; Temporarily increase GC threshold during startup
(setopt gc-cons-threshold most-positive-fixnum)

;; Restore to normal value after startup (e.g. 500MB)
(add-hook 'emacs-startup-hook
          (lambda () (setopt gc-cons-threshold (* 500 1024 1024))))

(setopt package-enable-at-startup nil)

(tool-bar-mode -1)
(scroll-bar-mode -1)

(setopt default-frame-alist
        '((height . 60)
          (width . 120)
          (left . 100)
          (top . 20)))
