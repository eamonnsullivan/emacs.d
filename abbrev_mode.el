;;; -*- lexical-binding: t -*-
;;; abbrev_mode.el --- stuff related to abbreviations

;; Copyright (c) 2017 Eamonn Sullivan

;; Author: Eamonn Sullivan <eamonn.sullivan@gmail.com>
;; Maintainer: Eamonn Sullivan <eamonn.sullivan@gmail.com>
;; Created 23 March 2017

;; Homepage: https://github.com/eamonnsullivan/emacs.d

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;;; Code:

;; turn on abbrev mode (uses the skeletons below)
(setq default-abbrev-mode t)

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

(define-skeleton eds-edeb-expansion-scala
  "Insert a skeleton for a debugging print statement. Leave point in the string."
  > "println(s\"EAMONN DEBUG: " _ "\")")

(define-skeleton eds-edump-expansion-javascript
  "Insert a skeleton for a debugging console log statement. With stringify."
  > "console.log(`EAMONN DEBUG: ${JSON.stringify(" _ ")}`);")

(define-skeleton eds-edeb-expansion-javascript
  "Insert a skeleton for a debugging console log statement."
  > "console.log(`EAMONN DEBUG: ${" _ "}`);")

(define-skeleton eds-edeb-expansion-java
  "Insert a skeleton for a debugging print statement. Leave point in the string."
  > "System.out.println(\"EAMONN DEBUG: " _ "\")")

(define-skeleton eds-short-documentation-block-scala
"Insert a documentation comment block."
>"/**"\n
"* " _ " "\n
"*"\n
"*/")

(define-skeleton eds-short-documentation-block-javascript
  "Insert a documentation comment block."
>"/**"\n
-2 "* " _ " "\n
"*"\n
"*/")

(define-skeleton eds-emsg-expansion
"Insert Bloomberg's standard emsg fprintf."
> "fprintf(stderr, \"emsg %s %d: " _ "\","\n
 "__FILE__, __LINE__);")

(define-skeleton eds-emsg-expansion-cpp
"C++ version of Bloomberg's standard emsg."
> "std::cerr << \"emsg: \" << __FILE__ << \" \" << __LINE__ << \" " _ "\" << std::endl;")

;; C++ style
(define-skeleton eds-if-expansion-cpp
"Insert a skeleton for a if statement. Leave point at the test." nil
> "if (" _ ")"\n
-4 "{"\n
-4 "}")

(define-skeleton eds-for-expansion-cpp
"Insert a for statement."
> "for (" _ "; ; )"\n
-4 "{"\n
-4 "}")

(define-skeleton eds-while-expansion-cpp
"Insert a while statement."
> "while (" _ ")"\n
-4 "{"\n
-4 "}")

(define-skeleton eds-documentation-block-cpp
"Insert an empty documentation block."
>"/**"\n
-2 "* @brief " _ " "\n
"*"\n
"* @param "\n
"*"\n
"* @returns "\n
"*/")

(define-skeleton eds-short-documentation-block-cpp
"Insert a short, in-line documenation block."
>"/**< " _ "*/")

(define-skeleton eds-bael-log-category-expansion
"Insert a BAEL_LOG_SET_CATEGORY statement."
> "BAEL_LOG_SET_CATEGORY(LOG_CATEGORY);")

(define-skeleton eds-bael-log-error-expansion
"Insert a BAEL_LOG_ERROR statement."
> "BAEL_LOG_ERROR << " _ ""\n
"<< BAEL_LOG_END;")

(define-skeleton eds-bael-log-debug-expansion
"Insert a BAEL_LOG_DEBUG statement."
> "BAEL_LOG_DEBUG << " _ ""\n
"<< BAEL_LOG_END;")

(define-skeleton eds-bael-log-info-expansion
"Insert a BAEL_LOG_INFO statement."
> "BAEL_LOG_INFO << " _ ""\n
"<< BAEL_LOG_END;")

(define-skeleton eds-bael-log-warn-expansion
"Insert a BAEL_LOG_WARN statement."
> "BAEL_LOG_WARN << " _ ""\n
"<< BAEL_LOG_END;")

(define-skeleton eds-bael-log-fatal-expansion
"Insert a BAEL_LOG_FATAL statement."
> "BAEL_LOG_FATAL << " _ ""\n
"<< BAEL_LOG_END;")

(define-skeleton eds-bael-log-trace-expansion
"Insert a BAEL_LOG_TRACE statement."
> "BAEL_LOG_TRACE << " _ ""\n
"<< BAEL_LOG_END;")

(define-skeleton eds-xml-annotation-expansion
"Insert an annotation/documentation block in a BAS schema."
>"<annotation>"\n
"<documentation>"\n
""_""\n
-2"</documentation>"\n
-2"</annotation>")

(setq skeleton-end-hook nil)

;; preserve case when using dabbrev
(setq dabbrev-case-replace nil)

;;; abbrev_mode.el ends here
