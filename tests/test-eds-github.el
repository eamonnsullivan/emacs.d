;;; test-eds-github.el --- Tests for eds-github -*- lexical-binding: t; -*-

;;; Commentary:

;; Buttercup tests for the eds-github library.

;;; Code:

(load-file "tests/setup.el")

(describe "eds-github"

  (describe "eds-github/--format-time"
    (it "formats an ISO timestamp"
      (let ((result (eds-github/--format-time "2025-01-15T10:30:00Z")))
        (expect result :to-match "2025-01-15")))

    (it "returns empty string for nil"
      (expect (eds-github/--format-time nil) :to-equal ""))

    (it "returns empty string for empty input"
      (expect (eds-github/--format-time "") :to-equal "")))

  (describe "eds-github/--status-face"
    (it "returns in-progress face for running jobs"
      (expect (eds-github/--status-face "in_progress" "")
              :to-equal 'eds-github/in-progress-face))

    (it "returns success face for completed success"
      (expect (eds-github/--status-face "completed" "success")
              :to-equal 'eds-github/success-face))

    (it "returns failure face for completed failure"
      (expect (eds-github/--status-face "completed" "failure")
              :to-equal 'eds-github/failure-face))

    (it "returns cancelled face for cancelled runs"
      (expect (eds-github/--status-face "completed" "cancelled")
              :to-equal 'eds-github/cancelled-face)))

  (describe "eds-github/--format-entry"
    (it "produces a tabulated-list row from a run alist"
      (let* ((run '((databaseId . 12345)
                    (name . "CI Build")
                    (status . "completed")
                    (conclusion . "success")
                    (headBranch . "main")
                    (event . "push")
                    (startedAt . "2025-01-15T10:30:00Z")))
             (entry (eds-github/--format-entry run)))
        (expect (car entry) :to-equal 12345)
        (expect (aref (cadr entry) 1) :to-equal "CI Build")
        (expect (aref (cadr entry) 2) :to-equal "main")
        (expect (aref (cadr entry) 3) :to-equal "push")))

    (it "handles missing fields gracefully"
      (let* ((run '((databaseId . 99)))
             (entry (eds-github/--format-entry run)))
        (expect (car entry) :to-equal 99)
        (expect (aref (cadr entry) 1) :to-equal "")
        (expect (aref (cadr entry) 2) :to-equal "")))

    (it "displays conclusion as status when run is completed"
      (let* ((run '((databaseId . 1)
                    (status . "completed")
                    (conclusion . "failure")))
             (entry (eds-github/--format-entry run))
             (status-cell (aref (cadr entry) 0)))
        (expect (substring-no-properties status-cell) :to-equal "failure")))

    (it "displays status directly when run is in progress"
      (let* ((run '((databaseId . 2)
                    (status . "in_progress")
                    (conclusion . "")))
             (entry (eds-github/--format-entry run))
             (status-cell (aref (cadr entry) 0)))
        (expect (substring-no-properties status-cell) :to-equal "in_progress"))))

  (describe "eds-github/--fetch-runs"
    (it "calls gh CLI and parses JSON output"
      (spy-on 'process-file
              :and-call-fake
              (lambda (&rest _args)
                (insert "[{\"databaseId\":1,\"name\":\"CI\",\"status\":\"completed\",\"conclusion\":\"success\",\"headBranch\":\"main\",\"event\":\"push\",\"startedAt\":\"2025-01-15T10:00:00Z\"}]")
                0))
      (let* ((eds-github/run-limit 7)
             (runs (eds-github/--fetch-runs "owner/repo")))
        (expect 'process-file :to-have-been-called-with
                "gh" nil t nil
                "run" "list"
                "--repo" "owner/repo"
                "--json" "databaseId,name,status,conclusion,headBranch,event,startedAt"
                "--limit" "7")
        (expect (length runs) :to-equal 1)
        (expect (alist-get 'name (aref runs 0)) :to-equal "CI")))

    (it "signals an error on empty output"
      (spy-on 'process-file :and-return-value 0)
      (expect (eds-github/--fetch-runs "owner/repo") :to-throw 'error))

    (it "signals an error with gh output when gh fails"
      (spy-on 'process-file
              :and-call-fake
              (lambda (&rest _args)
                (insert "authentication failed")
                1))
      (condition-case err
          (eds-github/--fetch-runs "owner/repo")
        (error
         (expect (error-message-string err) :to-match "authentication failed")))))

  (describe "eds-github/view-run-at-point"
    (it "opens the run in a browser via gh"
      (spy-on 'tabulated-list-get-id :and-return-value 42)
      (spy-on 'shell-command)
      (let ((eds-github/--current-repo "owner/repo"))
        (eds-github/view-run-at-point)
        (expect 'shell-command :to-have-been-called-with
                "gh run view 42 --repo owner/repo --web")))

    (it "escapes any dangerous characters in the shell command"
      (spy-on 'tabulated-list-get-id :and-return-value 42)
      (spy-on 'shell-command)
      (let ((eds-github/--current-repo "owner/repo;evil"))
        (eds-github/view-run-at-point)
        (expect 'shell-command :to-have-been-called-with
                "gh run view 42 --repo owner/repo\\;evil --web")))

    (it "signals an error when no run is at point"
      (spy-on 'tabulated-list-get-id :and-return-value nil)
      (expect (eds-github/view-run-at-point) :to-throw 'user-error)))

  (describe "eds-github/rerun-at-point"
    (it "reruns the workflow at point after confirmation"
      (spy-on 'tabulated-list-get-id :and-return-value 77)
      (spy-on 'yes-or-no-p :and-return-value t)
      (spy-on 'shell-command)
      (spy-on 'revert-buffer)
      (let ((eds-github/--current-repo "owner/repo"))
        (eds-github/rerun-at-point)
        (expect 'shell-command :to-have-been-called-with
                "gh run rerun 77 --repo owner/repo")
        (expect 'revert-buffer :to-have-been-called)))

    (it "does nothing when user declines confirmation"
      (spy-on 'tabulated-list-get-id :and-return-value 77)
      (spy-on 'yes-or-no-p :and-return-value nil)
      (spy-on 'shell-command)
      (spy-on 'revert-buffer)
      (let ((eds-github/--current-repo "owner/repo"))
        (eds-github/rerun-at-point)
        (expect 'shell-command :not :to-have-been-called)
        (expect 'revert-buffer :not :to-have-been-called)))

    (it "escapes any dangerous characters in the shell command"
      (spy-on 'tabulated-list-get-id :and-return-value 77)
      (spy-on 'yes-or-no-p :and-return-value t)
      (spy-on 'shell-command)
      (spy-on 'revert-buffer)
      (let ((eds-github/--current-repo "owner/repo;evil"))
        (eds-github/rerun-at-point)
        (expect 'shell-command :to-have-been-called-with
                "gh run rerun 77 --repo owner/repo\\;evil")))

    (it "signals an error when no run is at point"
      (spy-on 'tabulated-list-get-id :and-return-value nil)
      (expect (eds-github/rerun-at-point) :to-throw 'user-error)))

  (describe "eds-github/cancel-at-point"
    (it "cancels the workflow at point after confirmation"
      (spy-on 'tabulated-list-get-id :and-return-value 55)
      (spy-on 'yes-or-no-p :and-return-value t)
      (spy-on 'shell-command)
      (spy-on 'revert-buffer)
      (let ((eds-github/--current-repo "owner/repo"))
        (eds-github/cancel-at-point nil)
        (expect 'shell-command :to-have-been-called-with
                "gh run cancel 55 --repo owner/repo")
        (expect 'revert-buffer :to-have-been-called)))

    (it "passes --force when prefix argument is given"
      (spy-on 'tabulated-list-get-id :and-return-value 55)
      (spy-on 'yes-or-no-p :and-return-value t)
      (spy-on 'shell-command)
      (spy-on 'revert-buffer)
      (let ((eds-github/--current-repo "owner/repo"))
        (eds-github/cancel-at-point '(4))
        (expect 'shell-command :to-have-been-called-with
                "gh run cancel 55 --repo owner/repo --force")))

    (it "does nothing when user declines confirmation"
      (spy-on 'tabulated-list-get-id :and-return-value 55)
      (spy-on 'yes-or-no-p :and-return-value nil)
      (spy-on 'shell-command)
      (spy-on 'revert-buffer)
      (let ((eds-github/--current-repo "owner/repo"))
        (eds-github/cancel-at-point nil)
        (expect 'shell-command :not :to-have-been-called)
        (expect 'revert-buffer :not :to-have-been-called)))

    (it "escapes any dangerous characters in the shell command"
      (spy-on 'tabulated-list-get-id :and-return-value 55)
      (spy-on 'yes-or-no-p :and-return-value t)
      (spy-on 'shell-command)
      (spy-on 'revert-buffer)
      (let ((eds-github/--current-repo "owner/repo;evil"))
        (eds-github/cancel-at-point nil)
        (expect 'shell-command :to-have-been-called-with
                "gh run cancel 55 --repo owner/repo\\;evil")))

    (it "signals an error when no run is at point"
      (spy-on 'tabulated-list-get-id :and-return-value nil)
      (expect (eds-github/cancel-at-point nil) :to-throw 'user-error))))

(provide 'test-eds-github)
;;; test-eds-github.el ends here
