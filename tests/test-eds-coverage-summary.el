;;; test-eds-coverage-summary.el --- Tests for coverage summary -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Version: 0.1
;; Keywords: emacs testing coverage
;; URL: https://eamonnsullivan.co.uk

;;; Commentary:

;; Buttercup tests for the Undercover coverage summary.

;;; Code:

(load-file "tests/setup.el")
(require 'eds-coverage-summary)

(describe "eds-coverage-summary"
  (it "prints per-file and total coverage"
    (let ((report-file (make-temp-file "eds-coverage-" nil ".json")))
      (unwind-protect
          (progn
            (with-temp-file report-file
              (insert "{\"undercover.el\":{\"coverage\":{"
                      "\"/tmp/eds-one.el\":[null,2,0],"
                      "\"/tmp/eds-two.el\":[1,null,1]}}}"))
            (with-temp-buffer
              (let ((standard-output (current-buffer)))
                (eds-coverage-summary report-file))
              (expect (buffer-string) :to-match "eds-one.el[[:space:]]+1[[:space:]]+2[[:space:]]+50.0%")
              (expect (buffer-string) :to-match "Total[[:space:]]+3[[:space:]]+4[[:space:]]+75.0%")))
        (delete-file report-file))))

  (it "signals an error for an empty coverage report"
    (let ((report-file (make-temp-file "eds-coverage-" nil ".json")))
      (unwind-protect
          (progn
            (with-temp-file report-file
              (insert "{\"undercover.el\":{\"coverage\":{}}}"))
            (expect (eds-coverage-summary report-file) :to-throw 'error))
        (delete-file report-file))))

  (it "signals an error when no lines were executed"
    (let ((report-file (make-temp-file "eds-coverage-" nil ".json")))
      (unwind-protect
          (progn
            (with-temp-file report-file
              (insert "{\"undercover.el\":{\"coverage\":{"
                      "\"/tmp/eds-one.el\":[null,0,0]}}}"))
            (expect (eds-coverage-summary report-file) :to-throw 'error))
        (delete-file report-file)))))

(provide 'test-eds-coverage-summary)

;;; test-eds-coverage-summary.el ends here
