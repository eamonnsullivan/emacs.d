;;; eds-appearance.el --- Appearance compatibility helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; Keep Circadian's absolute timestamps compatible with timer APIs.

;;; Code:

(defun eds-appearance-circadian-absolute-time (time)
  "Return Circadian timestamp TIME in an unambiguous absolute-time format.
`run-at-time' treats numeric timestamps as relative delays.  Convert
`circadian-encode-time' results to lists, including on Emacs versions
where `encode-time' returns an integer."
  (time-convert time 'list))

(provide 'eds-appearance)
;;; eds-appearance.el ends here
