;; -*- coding: utf-8; lexical-binding: t -*-
(define-abbrev-table 'c++-mode-abbrev-table
  '(
    ("bbt" "BREG_BOOLEAN_TRUE" nil :count 0)
    ("blcat" "" eds-bael-log-category-expansion :count 8)
    ("bldebug" "" eds-bael-log-debug-expansion :count 10)
    ("blerror" "" eds-bael-log-error-expansion :count 9)
    ("blfatal" "" eds-bael-log-fatal-expansion :count 0)
    ("blinfo" "" eds-bael-log-info-expansion :count 0)
    ("bltrace" "" eds-bael-log-trace-expansion :count 3)
    ("blwarn" "" eds-bael-log-warn-expansion :count 0)
    ("docblock" "" eds-documentation-block-cpp :count 48)
    ("docshort" "" eds-short-documentation-block-cpp :count 7)
    ("edeb" "" eds-edeb-expansion-cpp :count 1)
    ("emsg" "" eds-emsg-expansion-cpp :count 1)
    ("forx" "" eds-for-expansion-cpp :count 3)
    ("ifx" "" eds-if-expansion-cpp :count 0)
    ("whilex" "" eds-while-expansion-cpp :count 0)
   ))

(define-abbrev-table 'clojure-mode-abbrev-table
  '(
    ("edeb" "" eds-edeb-expansion-clojure :count 1)
   ))

(define-abbrev-table 'fundamental-mode-abbrev-table
  '(
    ("doc" "annotation>
          <documentation>

          </documentation>
        </annotation>" nil :count 0)
    ("xmldoc" "" eds-xml-annotation-expansion :count 50)
   ))

(define-abbrev-table 'java-mode-abbrev-table
  '(
    ("edeb" "" eds-edeb-expansion-java :count 0)
   ))

(define-abbrev-table 'python-ts-mode-abbrev-table
  '(
    ("edeb" "" eds-edeb-expansion-python :count 0)
   ))

(define-abbrev-table 'scala-ts-mode-abbrev-table
  '(
    ("edeb" "" eds-edeb-expansion-scala :count 0)
    ("edocs" "" eds-short-documentation-block-scala :count 0)
   ))

