;;; -*- lexical-binding: t -*-
;;; init-abbrev.el --- stuff related to abbrev-mode

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

(define-skeleton eds-edeb-expansion-scala
  "Insert a skeleton for a debugging print statement. Leave point in the string."
  > "println(s\"EAMONN DEBUG: " _ "\")")

(define-skeleton eds-edeb-expansion-clojure
  "Insert a skeleton for a debugging print statement. Leave point in the string."
  > "(println \"EAMONN DEBUG:\" " _ " )")

(define-skeleton eds-edeb-expansion-java
  "Insert a skeleton for a debugging print statement. Leave point in the string."
  > "System.out.println(\"EAMONN DEBUG: " _ "\")")

(define-skeleton eds-edeb-expansion-python
  "Insert a skeleton for a debugging print statement. Leave point in the string."
  > "print(\"EAMONN DEBUG:\", " _ ")")

(setopt skeleton-end-hook nil)

;; preserve case when using dabbrev
(setopt dabbrev-case-replace nil)

(provide 'init-abbrev)
