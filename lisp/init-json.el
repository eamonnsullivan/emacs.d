;;; -*- lexical-binding: t -*-
;;; init-json.el --- stuff related to JSON

(use-package json-mode
  :ensure t
  :mode (("\\.json\\'" . json-mode)
         ("\\.tmpl\\'" . json-mode)
         ("\\.eslintrc\\'" . json-mode))
  :config (setq-default js-indent-level 2)
  (add-hook 'json-mode-hook
            (lambda()
              (local-unset-key (kbd "C-c C-f")))))

(use-package json-reformat
  :ensure t
  :after json-mode
  :bind (("C-c r" . json-pretty-print)
         ("C-c C-f" . json-pretty-print-buffer)))

(provide 'init-json)
