;;; early-init.el -- early initialization code for emacs  -*- lexical-binding: t; -*-

(setq gc-cons-threshold 100000000)
(setq package-enable-at-startup nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq default-frame-alist
       '((height . 60)
         (width . 120)
         (left . 100)
         (top . 20)))
