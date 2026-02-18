;;; -*- lexical-binding: t -*-
;;; init-json.el --- stuff related to JSON

(use-package json-mode
  :mode (("\\.json\\'" . json-ts-mode)
         ("\\.tmpl\\'" . json-ts-mode)
         ("\\.eslintrc\\'" . json-ts-mode))
  :config (setq-default js-indent-level 2)
  (add-hook 'json-mode-hook
            (lambda()
              (local-unset-key (kbd "C-c C-f"))
              (smartparens-mode t))))

(use-package json-reformat
  :after json-ts-mode
  :bind (("C-c r" . json-pretty-print)
         ("C-c C-f" . json-pretty-print-buffer)))

(provide 'init-json)
