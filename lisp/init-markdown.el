;;; init-markdown.el --- setup editing of markdown files. -*- lexical-binding: t -*-
;;
;; Copyright (C) 2026 Eamonnn Sullivan
;;
;;
;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Version: 1.0
;; Keywords: emacs markdown
;; URL: https://eamonnsullivan.co.uk
;;
;;
;;; Commentary:
;;
;;
;;; Code:

(require 'eds-blog)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setopt markdown-command "pandoc")
  :bind (("C-c C-e s" . eds-blog/make-svp-contact-link))
  :config
  (add-hook 'markdown-mode-hook 'flyspell-mode))

(provide 'init-markdown)
;;; init-markdown.el ends here
