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
      (expect (eds-github/cancel-at-point nil) :to-throw 'user-error)))

  ;; -----------------------------------------------------------
  ;; Pull Requests
  ;; -----------------------------------------------------------

  (describe "eds-github/--pr-state-face"
    (it "returns open face for OPEN state"
      (expect (eds-github/--pr-state-face "OPEN")
              :to-equal 'eds-github/pr-open-face))

    (it "returns closed face for CLOSED state"
      (expect (eds-github/--pr-state-face "CLOSED")
              :to-equal 'eds-github/pr-closed-face))

    (it "returns merged face for MERGED state"
      (expect (eds-github/--pr-state-face "MERGED")
              :to-equal 'eds-github/pr-merged-face))

    (it "returns default face for unknown state"
      (expect (eds-github/--pr-state-face "UNKNOWN")
              :to-equal 'default)))

  (describe "eds-github/--format-pr-entry"
    (it "produces a tabulated-list row from a PR alist"
      (let* ((pr '((number . 42)
                   (title . "Add feature X")
                   (state . "OPEN")
                   (author . ((login . "octocat")))
                   (headRefName . "feature-x")
                   (createdAt . "2025-01-10T09:00:00Z")
                   (updatedAt . "2025-01-15T14:30:00Z")))
             (entry (eds-github/--format-pr-entry pr)))
        (expect (car entry) :to-equal 42)
        (expect (aref (cadr entry) 0) :to-equal "42")
        (expect (substring-no-properties (aref (cadr entry) 1)) :to-equal "OPEN")
        (expect (aref (cadr entry) 2) :to-equal "Add feature X")
        (expect (aref (cadr entry) 3) :to-equal "octocat")
        (expect (aref (cadr entry) 4) :to-equal "feature-x")))

    (it "handles missing fields gracefully"
      (let* ((pr '((number . 1)))
             (entry (eds-github/--format-pr-entry pr)))
        (expect (car entry) :to-equal 1)
        (expect (aref (cadr entry) 2) :to-equal "")
        (expect (aref (cadr entry) 3) :to-equal "")
        (expect (aref (cadr entry) 4) :to-equal "")))

    (it "handles author as non-cons gracefully"
      (let* ((pr '((number . 5)
                   (author . :null)))
             (entry (eds-github/--format-pr-entry pr)))
        (expect (aref (cadr entry) 3) :to-equal ""))))

  (describe "eds-github/--fetch-prs"
    (it "calls gh CLI with correct arguments for open PRs"
      (spy-on 'process-file
              :and-call-fake
              (lambda (&rest _args)
                (insert "[{\"number\":1,\"title\":\"Fix bug\",\"state\":\"OPEN\",\"author\":{\"login\":\"dev\"},\"headRefName\":\"fix-bug\",\"createdAt\":\"2025-01-10T09:00:00Z\",\"updatedAt\":\"2025-01-15T14:30:00Z\"}]")
                0))
      (let* ((eds-github/pr-limit 25)
             (prs (eds-github/--fetch-prs "owner/repo" "open")))
        (expect 'process-file :to-have-been-called-with
                "gh" nil t nil
                "pr" "list"
                "--repo" "owner/repo"
                "--json" "number,title,state,author,headRefName,createdAt,updatedAt"
                "--state" "open"
                "--limit" "25")
        (expect (length prs) :to-equal 1)
        (expect (alist-get 'title (aref prs 0)) :to-equal "Fix bug")))

    (it "includes --author when author is provided"
      (spy-on 'process-file
              :and-call-fake
              (lambda (&rest _args)
                (insert "[]")
                0))
      (let ((eds-github/pr-limit 10))
        (condition-case nil
            (eds-github/--fetch-prs "owner/repo" "open" "octocat")
          (error nil))
        (expect (spy-calls-args-for 'process-file 0)
                :to-equal
                '("gh" nil t nil
                  "pr" "list"
                  "--repo" "owner/repo"
                  "--json" "number,title,state,author,headRefName,createdAt,updatedAt"
                  "--state" "open"
                  "--limit" "10"
                  "--author" "octocat"))))

    (it "does not include --author when author is nil"
      (spy-on 'process-file
              :and-call-fake
              (lambda (&rest _args)
                (insert "[{\"number\":1,\"title\":\"X\",\"state\":\"OPEN\",\"author\":{\"login\":\"a\"},\"headRefName\":\"b\",\"createdAt\":\"2025-01-10T09:00:00Z\",\"updatedAt\":\"2025-01-15T14:30:00Z\"}]")
                0))
      (let ((eds-github/pr-limit 10))
        (eds-github/--fetch-prs "owner/repo" "all" nil)
        (let ((args (spy-calls-args-for 'process-file 0)))
          (expect (member "--author" args) :to-be nil))))

    (it "signals an error on empty output"
      (spy-on 'process-file :and-return-value 0)
      (expect (eds-github/--fetch-prs "owner/repo" "open") :to-throw 'error))

    (it "signals an error when gh fails"
      (spy-on 'process-file
              :and-call-fake
              (lambda (&rest _args)
                (insert "not authenticated")
                1))
      (condition-case err
          (eds-github/--fetch-prs "owner/repo" "open")
        (error
         (expect (error-message-string err) :to-match "not authenticated")))))

  (describe "eds-github/pr-view-at-point"
    (it "opens the PR in a browser via gh"
      (spy-on 'tabulated-list-get-id :and-return-value 99)
      (spy-on 'shell-command)
      (let ((eds-github/--pr-repo "owner/repo"))
        (eds-github/pr-view-at-point)
        (expect 'shell-command :to-have-been-called-with
                "gh pr view 99 --repo owner/repo --web")))

    (it "escapes dangerous characters in the repo name"
      (spy-on 'tabulated-list-get-id :and-return-value 99)
      (spy-on 'shell-command)
      (let ((eds-github/--pr-repo "owner/repo;evil"))
        (eds-github/pr-view-at-point)
        (expect 'shell-command :to-have-been-called-with
                "gh pr view 99 --repo owner/repo\\;evil --web")))

    (it "signals an error when no PR is at point"
      (spy-on 'tabulated-list-get-id :and-return-value nil)
      (expect (eds-github/pr-view-at-point) :to-throw 'user-error)))

  (describe "eds-github/pr-filter-state"
    (it "updates the buffer-local state variable and reverts"
      (spy-on 'revert-buffer)
      (with-temp-buffer
        (setq-local eds-github/--pr-state "open")
        (setq-local eds-github/--pr-repo "owner/repo")
        (let ((completing-read-function
               (lambda (&rest _) "closed")))
          (eds-github/pr-filter-state "closed"))
        (expect eds-github/--pr-state :to-equal "closed")
        (expect 'revert-buffer :to-have-been-called))))

  (describe "eds-github/pr-filter-author"
    (it "sets author filter and reverts"
      (spy-on 'revert-buffer)
      (with-temp-buffer
        (setq-local eds-github/--pr-author nil)
        (setq-local eds-github/--pr-repo "owner/repo")
        (eds-github/pr-filter-author "octocat")
        (expect eds-github/--pr-author :to-equal "octocat")
        (expect 'revert-buffer :to-have-been-called)))

    (it "clears author filter when given empty string"
      (spy-on 'revert-buffer)
      (with-temp-buffer
        (setq-local eds-github/--pr-author "someone")
        (setq-local eds-github/--pr-repo "owner/repo")
        (eds-github/pr-filter-author "")
        (expect eds-github/--pr-author :to-be nil)
        (expect 'revert-buffer :to-have-been-called))))

  (describe "eds-github/pr-clear-filters"
    (it "resets all filters to defaults and reverts"
      (spy-on 'revert-buffer)
      (with-temp-buffer
        (setq-local eds-github/--pr-state "closed")
        (setq-local eds-github/--pr-author "someone")
        (setq-local eds-github/--pr-repo "owner/repo")
        (let ((eds-github/pr-default-state "open"))
          (eds-github/pr-clear-filters)
          (expect eds-github/--pr-state :to-equal "open")
          (expect eds-github/--pr-author :to-be nil)
          (expect 'revert-buffer :to-have-been-called))))))

(provide 'test-eds-github)
;;; test-eds-github.el ends here
