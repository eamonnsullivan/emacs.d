;;; -*- lexical-binding: t -*-
;;; init-yasnippet.el --- stuff related to yasnippet

(use-package yasnippet
  :diminish (yas-minor-mode . " Ⓨ")
  :hook ((prog-mode) . yas-minor-mode)
  :config
  (use-package yasnippet-snippets :after yasnippet :demand t)
  (add-to-list 'yas-snippet-dirs "~/.config/emacs/snippets")
  (yas-global-mode 1)
  (with-eval-after-load 'company-mode
    (add-to-list 'company-backends '(company-yasnippet)))
  (yas-reload-all))

(provide 'init-yasnippet)
