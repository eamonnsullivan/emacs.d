;;; -*- lexical-binding: t -*-
;;; init-java.el --- stuff related to coding in java

(use-package eclim
  :ensure t
  :init
  (setq eclimd-autostart t)
  (setq eclimd-executable "/Users/sullie09/eclipse/java-oxygen/Eclipse.app/Contents/Eclipse/eclimd")
  (add-hook 'java-mode-hook 'eclim-mode)
  (require 'eclimd)
  :config
  (setq help-at-pt-display-when-idle t)
  (setq help-at-pt-timer-delay 0.1)
  (help-at-pt-set-timer))

(add-hook 'java-mode-hook (lambda()
                            (setq c-basic-offset 4
                                  tab-width 4
                                  indent-tabs-mode nil)))

(use-package company-emacs-eclim
  :ensure t
  :init
  (company-emacs-eclim-setup)
  (global-company-mode t))

(provide 'init-java)
