;;; init-blogging.el --- Blogging workflow initialisation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan <me@eamonnsullivan.co.uk>

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Maintainer: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Created: 2026-01-06
;; Version: 0.1
;; Package-Requires: ((emacs "31.0"))
;; Keywords: blog, publishing, workflow
;; URL: https://github.com/eamonnsullivan/init-blogging

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides initialisation and configuration for blogging workflows
;; in Emacs, supporting publishing, editing, and organisation.

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

;; Need to handle difference in Emacs 31 and 32 on whether
;; markdown-ts-mode is loaded or not.
(if (require 'markdown-ts-mode nil :noerror)
    (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-ts-mode))
  (use-package markdown-mode
    :straight t
    :mode "\\.md\\'"))

(defun eds-blog/bind-svp-contact-key ()
  "Bind `C-c C-e s' to `eds-blog/make-svp-contact-link' locally."
  (local-set-key (kbd "C-c C-e s") #'eds-blog/make-svp-contact-link))

(add-hook 'markdown-ts-mode-hook #'eds-blog/bind-svp-contact-key)
(add-hook 'markdown-mode-hook #'eds-blog/bind-svp-contact-key)

(use-package ox-hugo
  :after ox)

(defun markdown-convert-buffer-to-org ()
  "Convert the current Markdown buffer to a sibling .org file using Pandoc."
  (interactive)
  (eds-blog/markdown-convert-buffer-to-org))

(provide 'init-blogging)

;;; init-blogging.el ends here
