;;; init-abbrev.el --- Abbrev mode initialisation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan <me@eamonnsullivan.co.uk>

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Maintainer: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Created: 2017-01-02
;; Version: 0.1
;; Package-Requires: ((emacs "31.0"))
;; Keywords: abbrev, convenience, editing
;; URL: https://github.com/eamonnsullivan/init-abbrev

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides initialisation and configuration for Abbrev mode,
;; enabling automatic expansion of abbreviations and improved editing workflow.

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


;; turn on abbrev mode (uses the skeletons below)
(setq-default abbrev-mode t)
(add-hook 'abbrev-mode-hook (lambda () (diminish 'abbrev-mode)))

;; skeletons -- these are loaded via abbrev mode. My abbrev_defs file
;; as entries like this:
;;
;; (define-abbrev-table 'c++-mode-abbrev-table '(
;;    ("bbt" "BREG_BOOLEAN_TRUE" nil 0)
;;    ("ifx" "" eds-if-expansion)
;;    ("whilex" "" eds-while-expansion)
;;    ("forx" "" eds-for-expansion)
;;    ("edeb" "" eds-edeb-expansion-cpp)
;;    ("emsg" "" eds-emsg-expansion-cpp)))

(define-skeleton eds-if-expansion
"Insert a skeleton for a if statement. Leave point at the test." nil
> "if (" _ ")"\n
-4 "{"\n
-4 "}")

(define-skeleton eds-for-expansion
"Insert a for statement."
> "for (" _ "; ; )"\n
-4 "{"\n
-4 "}")

(define-skeleton eds-while-expansion
"Insert a while statement."
> "while (" _ ")"\n
-4 "{"\n
-4 "}")

(define-skeleton eds-edeb-expansion
"Insert my standard debugging fprintf."
> "fprintf(stderr, \"EAMONN DEBUG: " _ "\");")

(define-skeleton eds-edeb-expansion-cpp
"Insert my standard debugging message in C++."
> "std::cerr << \"EAMONN DEBUG: " _ "\" << std::endl;")

(define-skeleton eds-edeb-expansion-clojure
  "Insert a skeleton for a debugging print statement. Leave point in the string."
  > "(println \"EAMONN DEBUG:\" " _ " )")

(define-skeleton eds-edeb-expansion-java
  "Insert a skeleton for a debugging print statement. Leave point in the string."
  > "System.out.println(\"EAMONN DEBUG: " _ "\")")

(setopt skeleton-end-hook nil)

;; preserve case when using dabbrev
(setopt dabbrev-case-replace nil)

(provide 'init-abbrev)
;;; init-abbrev.el ends here
