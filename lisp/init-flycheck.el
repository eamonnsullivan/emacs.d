;;; init-flycheck.el --- Flycheck mode initialisation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan <me@eamonnsullivan.co.uk>

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Maintainer: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Created: 2017-02-27
;; Version: 0.1
;; Package-Requires: ((emacs "24.3"))
;; Keywords: flycheck, syntax, checking, tools
;; URL: https://github.com/eamonnsullivan/init-flycheck

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides initialisation and configuration for Flycheck,
;; enabling real-time syntax checking and improved coding workflow in Emacs.

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

(use-package flycheck-pos-tip
  :after flycheck)

(use-package flycheck
  :straight t
  :config
  (flycheck-add-mode 'scala-scalastyle 'scala-ts-mode)
  (setq-default flycheck-disabled-checkers '(org-lint))
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck-eglot
  :straight
  (:host github :repo "flycheck/flycheck-eglot" :files ("*.el"))
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

(use-package flycheck-clj-kondo
  :if (executable-find "clj-kondo")
  :after clojure-mode
  :hook (clojure-mode . (lambda () (require 'flycheck-clj-kondo))))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
