;;; -*- lexical-binding: t -*-
;;; init-markdown.el --- settings related to markdown mode

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "pandoc")
  :config
  (add-hook 'markdown-mode-hook 'flyspell-mode))

(provide 'init-markdown)

;;; markdown.el ends here
