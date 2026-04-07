;;; early-init.el -- early initialization code for emacs  -*- lexical-binding: t; -*-

;; Temporarily increase GC threshold during startup
(setopt gc-cons-threshold most-positive-fixnum)

;; Restore to normal value after startup (e.g. 500MB)
(add-hook 'emacs-startup-hook
          (lambda () (setopt gc-cons-threshold (* 500 1024 1024))))

(setopt package-enable-at-startup nil)

(tool-bar-mode -1)
(scroll-bar-mode -1)

(setopt initial-frame-alist `((horizontal-scroll-bars . nil)
                              (menu-bar-lines . 0)
                              (tool-bar-lines . 0)
                              (vertical-scroll-bars . nil)
                              (width . (text-pixels . 800))
                              (height . (text-pixels . 900))
                              (list '(undecorated . t))
                              (border-width . 0)))

(add-hook 'after-init-hook
          (lambda ()
            (setopt default-frame-alist `((horizontal-scroll-bars . nil)
                                          (menu-bar-lines . 0)
                                          (tool-bar-lines . 0)
                                          (vertical-scroll-bars . nil)
                                          (width . (text-pixels . 800))
                                          (height . (text-pixels . 900))
                                          (list '(undecorated . t))
                                          (border-width . 0)))))

;; Single VC backend inscreases booting speed
(setq vc-handled-backends '(Git))
;; (setopt user-lisp-directory (locate-user-emacs-file "eds-lisp/"))
