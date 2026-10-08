;;; test-eds-appearance.el --- Appearance compatibility tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Exercise Circadian's timestamp boundary with the real timer API.

;;; Code:

(require 'buttercup)
(require 'eds-appearance)

(describe "eds-appearance Circadian absolute timestamps"
  (it "schedules integer timestamps at the requested time, not years later"
    (let* ((target (time-convert (time-add (current-time) 3600) 'integer))
           (timer (run-at-time (eds-appearance-circadian-absolute-time target)
                               nil #'ignore)))
      (unwind-protect
          (expect (time-equal-p (timer--time timer) target) :to-be-truthy)
        (cancel-timer timer))))

  (it "preserves legacy list timestamps"
    (let* ((target (time-convert (time-add (current-time) 3600) 'list))
           (timer (run-at-time (eds-appearance-circadian-absolute-time target)
                               nil #'ignore)))
      (unwind-protect
          (expect (time-equal-p (timer--time timer) target) :to-be-truthy)
        (cancel-timer timer)))))

(provide 'test-eds-appearance)
;;; test-eds-appearance.el ends here
