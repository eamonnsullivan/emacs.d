;;; -*- lexical-binding: t -*-
;;; init-js.el --- stuff related to Javascript

(use-package nvm
  :commands (nvm-use nvm-use-for nvm--installed-versions))

(use-package rjsx-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.js$" . rjsx-mode))
  :config
  (add-hook 'rjsx-mode-hook
            (lambda()
              (setq js2-basic-offset 2
                    js2-highlight-level 3
                    js2-bounce-indent-p t
                    indent-tabs-mode nil
                    js-indent-level 2
                    js2-strict-missing-semi-warning nil)
              (tern-mode)
              (js2-imenu-extras-mode)
              (js2-refactor-mode)
              (js2r-add-keybindings-with-prefix "C-c C-r")
              (auto-revert-mode)
              (flycheck-mode 1))))

(use-package js-comint
  :ensure t
  :config
  (add-hook 'js2-mode-hook
            (lambda ()
              (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
              (local-set-key (kbd "C-M-x") 'js-send-last-sexp-and-go)
              (local-set-key (kbd "C-c b") 'js-send-buffer)
              (local-set-key (kbd "C-c C-b") 'js-send-buffer-and-go)
              (local-set-key (kbd "C-c l") 'js-load-file-and-go))))

(use-package js2-refactor
  :ensure t)
(use-package js2-mode
  :ensure t
  :defer t
  :config
  (setq-default js2-ignored-warnings '("msg.extra.trailing.comma"))
  (add-hook 'js2-mode-hook
            (lambda()
              (setq js2-basic-offset 2
                    js2-highlight-level 3
                    js2-bounce-indent-p t
                    indent-tabs-mode nil
                    js-indent-level 2
                    js2-strict-missing-semi-warning nil)
              (tern-mode)
              (js2-imenu-extras-mode)
              (js2-refactor-mode)
              (js2r-add-keybindings-with-prefix "C-c C-r")
              (auto-revert-mode)
              (flycheck-mode 1))))

(use-package add-node-modules-path
   :ensure t
   :config
   (add-hook 'js2-mode-hook 'add-node-modules-path)
   (add-hook 'rjsx-mode-hook 'add-node-modules-path))

(defun delete-tern-process()
  (interactive)
  (delete-process "Tern"))

(use-package prettier-js
  :ensure t
  :diminish prettier-js-mode
  :config
  (setq prettier-js-args '(
                           "--arrow-parens" "always"
                           "--tab-width" "2"
                           "--use-tabs" "false"
                           "--trailing-comma" "all")))

(use-package eslint-fix
  :ensure t
  :config
  (eval-after-load 'js2-mode
    '(add-hook 'js2-mode-hook
               (lambda()
                 (add-hook 'after-save-hook 'eslint-fix nil t))))
  (eval-after-load 'rjsx-mode
    '(add-hook 'rjsx-mode-hook
               (lambda()
                 (add-hook 'after-save-hook 'eslint-fix nil t)))))

(provide 'init-js)