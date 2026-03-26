;;; init-markdown.el --- Markdown mode initialisation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan <me@eamonnsullivan.co.uk>

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Maintainer: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Created: 2018-05-03
;; Version: 0.1
;; Package-Requires: ((emacs "24.4"))
;; Keywords: markdown, languages, tools
;; URL: https://github.com/eamonnsullivan/init-markdown

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides initialisation and configuration for Markdown mode,
;; supporting editing, syntax highlighting, and workflow enhancements for Markdown files in Emacs.

;;; Licence:

;; This programme is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public Licence as published by
;; the Free Software Foundation, either version 3 of the Licence, or
;; (at your option) any later version.

;; This programme is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public Licence for more details.

;; You should have received a copy of the GNU General Public Licence
;; along with this programme.  If not, see <https://www.gnu.org/licenses/>.

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

;; from http://yummymelon.com/devnull/import-markdown-to-org-with-the-clipboard-in-emacs.html
(defun cc/yank-markdown-as-org ()
  "Yank Markdown text as Org.

This command will convert Markdown text in the top of the `kill-ring'
and convert it to Org using the pandoc utility."
  (interactive)
  (save-excursion
    (with-temp-buffer
      (yank)
      (shell-command-on-region
       (point-min) (point-max)
       "pandoc -f markdown -t org --wrap=preserve" t t)
      (kill-region (point-min) (point-max)))
    (yank)))

(provide 'init-markdown)
;;; init-markdown.el ends here
