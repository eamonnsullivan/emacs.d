;;; eds-package-usage.el --- Track interactive package usage -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Version: 0.1
;; Keywords: convenience, packages

;;; Commentary:

;; Record privacy-safe aggregates for interactive commands provided by
;; straight.el packages.  Use `eds-package-usage-report' after collecting
;; representative data for several weeks.  A package with no recorded command
;; use still needs manual review for modes, hooks, themes, and dependencies.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)

(defgroup eds-package-usage nil
  "Track aggregate use of commands from straight.el packages."
  :group 'convenience)

(defcustom eds-package-usage-state-file
  (locate-user-emacs-file "var/package-usage.el")
  "File in which aggregate package usage is stored."
  :type 'file)

(defcustom eds-package-usage-save-interval 900
  "Seconds of idle time before package usage is saved."
  :type 'integer)

(defcustom eds-package-usage-attribution-overrides nil
  "Alist mapping command symbols to package names.
A nil package name means that the command should not be recorded."
  :type '(alist :key-type symbol :value-type (choice string (const nil))))

(defconst eds-package-usage--state-version 1)

(defvar eds-package-usage--records (make-hash-table :test #'eq))
(defvar eds-package-usage--origin-cache (make-hash-table :test #'eq))
(defvar eds-package-usage--save-timer nil)
(defvar eds-package-usage--dirty nil)

(defun eds-package-usage--straight-root ()
  "Return the active straight.el root directory."
  (file-name-as-directory
   (expand-file-name "straight"
                     (or (bound-and-true-p straight-base-dir)
                         user-emacs-directory))))

(defun eds-package-usage--package-from-file (file)
  "Return the straight package owning FILE, or nil.
Both build and repository paths are recognised."
  (when file
    (let* ((root (regexp-quote (eds-package-usage--straight-root)))
           (case-fold-search nil)
           (expanded-file (expand-file-name file))
           (pattern (concat "\\`" root
                            "\\(?:build[^/]*/\\|repos/\\)"
                            "\\([^/]+\\)/")))
      (when (string-match pattern expanded-file)
        (match-string-no-properties 1 expanded-file)))))

(defun eds-package-usage--command-file (command)
  "Return the source or autoload file associated with COMMAND."
  (let ((file (or (symbol-file command 'defun)
                  (let ((definition (and (fboundp command)
                                         (symbol-function command))))
                    (when (autoloadp definition)
                      (nth 1 definition))))))
    (when (stringp file)
      (if (file-name-absolute-p file)
          file
        (locate-library file))))

(defun eds-package-usage--command-package (command)
  "Return the straight package attributed to COMMAND, or nil."
  (let ((override (assq command eds-package-usage-attribution-overrides)))
    (if override
        (cdr override)
      (let ((cached (gethash command eds-package-usage--origin-cache 'missing)))
        (if (not (eq cached 'missing))
            cached
          (let ((package
                 (eds-package-usage--package-from-file
                  (eds-package-usage--command-file command))))
            (puthash command package eds-package-usage--origin-cache)
            package))))))

(defun eds-package-usage--record (command package &optional now)
  "Record one invocation of COMMAND attributed to PACKAGE at NOW."
  (let* ((time (or now (current-time)))
         (stamp (float-time time))
         (day (format-time-string "%Y-%m-%d" time))
         (record (copy-sequence (gethash command eds-package-usage--records)))
         (days (copy-sequence (plist-get record :days))))
    (unless (member day days)
      (push day days))
    (setq record (list :package package
                       :count (1+ (or (plist-get record :count) 0))
                       :first (or (plist-get record :first) stamp)
                       :last stamp
                       :days days))
    (puthash command record eds-package-usage--records)
    (setq eds-package-usage--dirty t)))

(defun eds-package-usage--pre-command ()
  "Record the package owning the command about to run."
  (let* ((command (or real-this-command this-command))
         (package (and (symbolp command)
                       (commandp command)
                       (eds-package-usage--command-package command))))
    (when package
      (eds-package-usage--record command package))))

(defun eds-package-usage--serializable-records ()
  "Return usage records as a stable, serialisable alist."
  (let (records)
    (maphash (lambda (command record)
               (push (cons command record) records))
             eds-package-usage--records)
    (sort records (lambda (left right)
                    (string< (symbol-name (car left))
                             (symbol-name (car right)))))))

(defun eds-package-usage-save ()
  "Atomically save package usage aggregates when they have changed."
  (interactive)
  (when eds-package-usage--dirty
    (let* ((target (expand-file-name eds-package-usage-state-file))
           (directory (file-name-directory target))
           temporary)
      (make-directory directory t)
      (setq temporary (make-temp-file
                       (expand-file-name ".package-usage-" directory)))
      (unwind-protect
          (progn
            (with-temp-file temporary
              (let ((print-length nil)
                    (print-level nil))
                (prin1 (list :version eds-package-usage--state-version
                             :records
                             (eds-package-usage--serializable-records))
                       (current-buffer))))
            (rename-file temporary target t)
            (setq temporary nil
                  eds-package-usage--dirty nil))
        (when (and temporary (file-exists-p temporary))
          (delete-file temporary))))))

(defun eds-package-usage-load ()
  "Load previously saved usage aggregates.
Invalid or unsupported state is ignored without interrupting startup."
  (interactive)
  (when (file-readable-p eds-package-usage-state-file)
    (condition-case err
(let ((state (with-temp-buffer
               (insert-file-contents eds-package-usage-state-file)
               (let ((read-eval nil))
                 (read (current-buffer))))))
          (when (equal (plist-get state :version)
                       eds-package-usage--state-version)
            (clrhash eds-package-usage--records)
            (dolist (entry (plist-get state :records))
              (when (and (symbolp (car-safe entry))
                         (listp (cdr-safe entry)))
                (puthash (car entry) (cdr entry)
                         eds-package-usage--records)))
            (setq eds-package-usage--dirty nil)))
      (error
       (message "Could not load package usage data: %s"
                (error-message-string err))))))

(defun eds-package-usage--build-directories ()
  "Return straight build directories that exist."
  (let ((root (eds-package-usage--straight-root)))
    (when (file-directory-p root)
      (seq-filter
       (lambda (path)
         (and (file-directory-p path)
              (string-match-p "/build[^/]*/?\\'" path)))
       (directory-files root t "\\`build" t)))))

(defun eds-package-usage--installed-packages ()
  "Return package names represented in straight build directories."
  (let (packages)
    (dolist (build (eds-package-usage--build-directories))
      (dolist (path (directory-files build t
                                     directory-files-no-dot-files-regexp t))
        (when (file-directory-p path)
          (push (file-name-nondirectory path) packages))))
    (sort (delete-dups packages) #'string<)))

(defun eds-package-usage--package-summary ()
  "Return a hash table of package-level usage summaries."
  (let ((summary (make-hash-table :test #'equal)))
    (maphash
     (lambda (command record)
       (let* ((package (plist-get record :package))
              (current (gethash package summary))
              (count (+ (or (plist-get current :count) 0)
                        (plist-get record :count)))
              (days (delete-dups
                     (append (plist-get record :days)
                             (plist-get current :days))))
              (last (max (or (plist-get current :last) 0)
                         (or (plist-get record :last) 0)))
              (commands (cons command (plist-get current :commands))))
         (puthash package
                  (list :count count :days days :last last
                        :commands commands)
                  summary)))
     eds-package-usage--records)
    summary))

(defun eds-package-usage--report-entries ()
  "Return tabulated entries for installed and observed packages."
  (let ((summary (eds-package-usage--package-summary))
        packages)
    (setq packages (eds-package-usage--installed-packages))
    (maphash (lambda (package _record)
               (push package packages))
             summary)
    (mapcar
     (lambda (package)
       (let* ((record (gethash package summary))
              (count (or (plist-get record :count) 0))
              (days (length (plist-get record :days)))
              (last (plist-get record :last))
              (commands (mapcar #'symbol-name
                                (seq-take (plist-get record :commands) 3))))
         (list package
               (vector package
                       (number-to-string count)
                       (number-to-string days)
                       (if last
                           (format-time-string "%Y-%m-%d"
                                               (seconds-to-time last))
                         "never")
                       (string-join commands ", ")
                       (if record "observed" "review")))))
     (sort (delete-dups packages) #'string<))))

(defun eds-package-usage--numeric-column-less-p (left right column)
  "Return non-nil when LEFT's numeric COLUMN is less than RIGHT's."
  (< (string-to-number (aref (cadr left) column))
     (string-to-number (aref (cadr right) column))))

(defun eds-package-usage--calls-less-p (left right)
  "Return non-nil when LEFT has fewer calls than RIGHT."
  (eds-package-usage--numeric-column-less-p left right 1))

(defun eds-package-usage--days-less-p (left right)
  "Return non-nil when LEFT has fewer distinct days than RIGHT."
  (eds-package-usage--numeric-column-less-p left right 2))

(define-derived-mode eds-package-usage-report-mode tabulated-list-mode
  "Package Usage"
  "Display aggregate use of straight.el packages."
  (setq tabulated-list-format
        [("Package" 28 t)
         ("Calls" 10 eds-package-usage--calls-less-p :right-align t)
         ("Days" 8 eds-package-usage--days-less-p :right-align t)
         ("Last used" 12 t)
         ("Example commands" 42 t)
         ("Status" 10 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Last used" . nil))
  (add-hook 'tabulated-list-revert-hook
            #'eds-package-usage--refresh-report nil t)
  (tabulated-list-init-header))

(defun eds-package-usage--refresh-report ()
  "Refresh the current package usage report."
  (setq tabulated-list-entries (eds-package-usage--report-entries)))

(defun eds-package-usage-report ()
  "Show package usage and packages needing manual review.
A `review' status is not proof that a package is unused: check passive
modes, hooks, themes, integrations, and dependencies before removal."
  (interactive)
  (eds-package-usage-save)
  (let ((buffer (get-buffer-create "*Package Usage*")))
    (with-current-buffer buffer
      (eds-package-usage-report-mode)
      (eds-package-usage--refresh-report)
      (tabulated-list-print t))
    (pop-to-buffer buffer)))

(defun eds-package-usage-reset ()
  "Delete all recorded usage after confirmation."
  (interactive)
  (when (yes-or-no-p "Delete all package usage history? ")
    (clrhash eds-package-usage--records)
    (setq eds-package-usage--dirty t)
    (eds-package-usage-save)
    (message "Package usage history reset")))

;;;###autoload
(define-minor-mode eds-package-usage-mode
  "Globally record aggregate use of straight.el package commands."
  :global t
  :group 'eds-package-usage
  (if eds-package-usage-mode
      (progn
        (eds-package-usage-load)
        (add-hook 'pre-command-hook #'eds-package-usage--pre-command)
        (add-hook 'kill-emacs-hook #'eds-package-usage-save)
        (when (timerp eds-package-usage--save-timer)
          (cancel-timer eds-package-usage--save-timer))
        (setq eds-package-usage--save-timer
              (run-with-idle-timer eds-package-usage-save-interval t
                                   #'eds-package-usage-save)))
    (remove-hook 'pre-command-hook #'eds-package-usage--pre-command)
    (remove-hook 'kill-emacs-hook #'eds-package-usage-save)
    (when (timerp eds-package-usage--save-timer)
      (cancel-timer eds-package-usage--save-timer))
    (setq eds-package-usage--save-timer nil)
    (eds-package-usage-save)))

(provide 'eds-package-usage)

;;; eds-package-usage.el ends here
