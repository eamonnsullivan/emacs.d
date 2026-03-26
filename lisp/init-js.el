;;; init-js.el --- JavaScript and Typescript mode initialisation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan <me@eamonnsullivan.co.uk>

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Maintainer: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Created: 2018-05-05
;; Version: 0.1
;; Package-Requires: ((emacs "31.0"))
;; Keywords: javascript, languages, tools
;; URL: https://github.com/eamonnsullivan/init-js

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides initialisation and configuration for JavaScript mode,
;; supporting editing, syntax highlighting, and workflow enhancements for JavaScript programming in Emacs.

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
  (typescript-ts-mode . prettier-mode)
  :config
  (setopt prettier-mode-sync-config-flag nil))

(use-package eslintd-fix
  :hook ((typescript-ts-base-mode . eslintd-fix-mode)))

(defun eds/js-ts-defaults ()
  "Set up defaults for JavaScript editing."
  (subword-mode +1)
  (rainbow-mode +1))


(provide 'init-js)
;;; init-js.el ends here
