;;; -*- lexical-binding: t -*-
;;; init-js.el --- stuff related to Javascript/Typescript/TSX

(use-package nvm
  :commands (nvm-use nvm-use-for nvm--installed-versions))

(use-package typescript-ts-mode
  :init
  (progn
    (add-to-list
     'auto-mode-alist
     (cons "\\.js$" (defun choose-js-type-mode ()
                     (save-excursion
                       (goto-char (point-min))
                       (let ((buff (current-buffer)))
                         (if (or (search-forward "React." nil t 1)
                                 (search-forward "import React" nil t 1))
                             (tsx-ts-mode)
                           (typescript-ts-mode)))))))))

(use-package js-comint
  :config
  (add-hook 'js-ts-mode-hook
            (lambda ()
              (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
              (local-set-key (kbd "C-M-x") 'js-send-last-sexp-and-go)
              (local-set-key (kbd "C-c b") 'js-send-buffer)
              (local-set-key (kbd "C-c C-b") 'js-send-buffer-and-go))))

(use-package add-node-modules-path
   :config
   (add-hook 'typescript-ts-mode-hook 'add-node-modules-path)
   (add-hook 'tsx-ts-mode-hook 'add-node-modules-path))

(use-package prettier
  :hook
  (typescript-mode . prettier-mode)
  :config
  (setq prettier-mode-sync-config-flag nil))

(use-package eslintd-fix
  :hook ((typescript-mode . eslintd-fix-mode)
         (tsx-ts-mode . eslintd-fix-mode)))


(provide 'init-js)
