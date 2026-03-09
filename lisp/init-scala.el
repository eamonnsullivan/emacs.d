;;; init-scala.el --- Scala mode initialisation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan <me@eamonnsullivan.co.uk>

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Maintainer: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Created: 2018-05-03
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: scala, languages, tools
;; URL: https://github.com/eamonnsullivan/init-scala

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides initialisation and configuration for Scala mode,
;; supporting editing, syntax highlighting, and workflow enhancements for Scala programming in Emacs.

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

(use-package scala-ts-mode
  :mode (("\.scala$" . scala-ts-mode)))

(use-package sbt-mode
  :straight
  (sbt-mode :type git :host github :repo "hvesalai/emacs-sbt-mode")
  :commands sbt-start sbt-command
  :bind (("C-c C-b" . sbt-hydra)))

;; feature-mode
(use-package feature-mode
  :config
  (defvar feature-step-search-path "src/test/scala/steps/**/*Steps.scala"))

(provide 'init-scala)
;;; init-scala.el ends here
