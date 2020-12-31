;;; minimal-init.el --- Initialization code for emacs when I want to strip everything.

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(require 'use-package)

(use-package use-package-chords
  :ensure t
  :init
  (setq key-chord-two-keys-delay 0.05)
  :config
  (key-chord-mode 1)
  (key-chord-define-global "JJ" 'previous-buffer))
