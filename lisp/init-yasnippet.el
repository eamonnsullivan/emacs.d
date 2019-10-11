;;; -*- lexical-binding: t -*-
;;; init-yasnippet.el --- stuff related to yasnippet

(use-package yasnippet
  :diminish (yas-minor-mode . " Ⓨ")
  :hook ((prog-mode text-mode) . yas-minor-mode)
  :config
  (use-package yasnippet-snippets :after yasnippet :demand t)
  (with-eval-after-load 'company-mode
    (add-to-list 'company-backends '(company-yasnippet)))
  (yas-reload-all))

(provide 'init-yasnippet)
