;;; eds-coverage-summary.el --- Summarize Undercover output -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Version: 0.1
;; Keywords: emacs testing coverage
;; URL: https://eamonnsullivan.co.uk

;;; Commentary:

;; Read Undercover's SimpleCov JSON output and print a concise coverage table.

;;; Code:

(require 'json)

(defun eds-coverage-summary--file-statistics (file-name line-hits)
  "Return coverage statistics for FILE-NAME and LINE-HITS."
  (let ((relevant 0)
        (covered 0))
    (dolist (hits line-hits)
      (when (numberp hits)
        (setq relevant (1+ relevant))
        (when (> hits 0)
          (setq covered (1+ covered)))))
    (list (file-name-nondirectory file-name) covered relevant)))

(defun eds-coverage-summary (report-file)
  "Print a summary of the Undercover SimpleCov REPORT-FILE.
Signal an error when the report contains no files or no executed lines."
  (unless (file-readable-p report-file)
    (error "Coverage report does not exist: %s" report-file))
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (report (json-read-file report-file))
         (undercover-report (gethash "undercover.el" report))
         (coverage (and (hash-table-p undercover-report)
                        (gethash "coverage" undercover-report)))
         rows)
    (unless (and (hash-table-p coverage)
                 (> (hash-table-count coverage) 0))
      (error "Coverage report contains no instrumented files"))
    (maphash (lambda (file-name line-hits)
               (push (eds-coverage-summary--file-statistics file-name line-hits)
                     rows))
             coverage)
    (setq rows (sort rows (lambda (left right)
                            (string-lessp (car left) (car right)))))
    (let ((total-covered 0)
          (total-relevant 0))
      (princ (format "\nCoverage report: %s\n\n" report-file))
      (princ (format "%-20s %8s %8s %8s\n"
                     "File" "Covered" "Relevant" "Coverage"))
      (princ (make-string 47 ?-))
      (princ "\n")
      (dolist (row rows)
        (pcase-let ((`(,file-name ,covered ,relevant) row))
          (setq total-covered (+ total-covered covered)
                total-relevant (+ total-relevant relevant))
          (princ (format "%-20s %8d %8d %7.1f%%\n"
                         file-name covered relevant
                         (if (> relevant 0)
                             (* 100.0 (/ (float covered) relevant))
                           0.0)))))
      (princ (make-string 47 ?-))
      (princ "\n")
      (princ (format "%-20s %8d %8d %7.1f%%\n"
                     "Total" total-covered total-relevant
                     (if (> total-relevant 0)
                         (* 100.0 (/ (float total-covered) total-relevant))
                       0.0)))
      (when (zerop total-covered)
        (error "Coverage report contains no executed lines")))))

(provide 'eds-coverage-summary)

;;; eds-coverage-summary.el ends here
