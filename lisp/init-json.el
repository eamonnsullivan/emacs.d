;;; -*- lexical-binding: t -*-
;;; init-json.el --- stuff related to JSON

(use-package json-mode
  :mode (("\\.json\\'" . json-mode)
         ("\\.tmpl\\'" . json-mode)
         ("\\.eslintrc\\'" . json-mode))
  :config (setq-default js-indent-level 2)
  (add-hook 'json-mode-hook
            (lambda()
              (local-unset-key (kbd "C-c C-f"))
              (smartparens-mode t))))

(use-package json-reformat
  :after json-mode
  :bind (("C-c r" . json-pretty-print)
         ("C-c C-f" . json-pretty-print-buffer)))

(use-package json-navigator
  :after json-mode)

(provide 'init-json)
