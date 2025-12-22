;;; eds.el --- Library of small functions and tweaks to various packages  -*- lexical-binding:t -*-

;; Copyright (C) 2025 Eamonnn Sullivan


;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Version: 1.0
;; Keywords: emacs testing development
;; URL: https://eamonnsullivan.co.uk


;;; Commentary:

(defun eds/extract-jira-ticket (input)
  (cond
   ((string-match "[A-Za-z]+-[0-9]+" input) (upcase (match-string 0 input)))
   ((string-match "^innovation+" input) "INNOVATION-DAY")
   ((string-match "^cop-" input) "COP-DAY")
   (t "NO-TICKET")))

(defun eds/insert-git-branch-name ()
  "Insert the current git branch name at point, surrounded by square brackets.
We use this to make the jira tickets easy to spot in the commit messages."
  (interactive)
  (when eds-insert-branch-name-p
    (setq eds-insert-branch-name-p nil)
    (insert
     "["
     (eds/extract-jira-ticket
      (magit-get-current-branch))
     "] ")))

(defun eds/kill-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (if (use-region-p)
      (delete-region
       (region-beginning)
       (region-end))
    (delete-region
     (point)
     (progn
       (forward-word arg)
       (point)))))

(defun eds/backward-kill-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (eds/kill-word (- arg)))

(defun eds/switch-to-previous-buffer ()
  "Switch to previously open buffer.
  Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer
   (other-buffer
    (current-buffer)
    1)))

(defun eds/open-buffer-on-desktop ()
  "Open the current directory in Mac OS X finder, Nautilus or Explorer"
  (interactive)
  (let ((file (buffer-file-name))
        (cmd (cond ((eq system-type 'darwin)
                    "open")
                   ((eq system-type 'gnu/linux)
                    "nautilus")
                   ((eq system-type 'windows-nt)
                    "explorer")
                   ;; haven't tested this
                   (t nil))))
    (if file
        (if cmd
            (shell-command
             (format
              "%s %s"
              (executable-find cmd)
              (file-name-directory file)))
          (display-warning
           :error "Unknown system type"))
      (display-warning
       :error "Buffer not attached to any file"))))

(defun eds/insert-skeleton-blog-post (title)
  "Insert a basic skeleton for a blog post."
  (goto-char (point-min))
  (insert (format "{:title \"%s\"\n :layout :post\n :tags []}\n\n" title)))

(defun eds/strip-invalid-chars (title)
  "Strip characters that don't work in filenames or github branches"
  (replace-regexp-in-string "[\\?\\>\\<\\|\\:\\&\\*\/\\!]" "" title))

(defun eds/process-title (title)
  "Downcase and hyphenate a title case string. Remove characters
that don't work in a filename."
  (eds/strip-invalid-chars (mapconcat 'identity (split-string (downcase title)) "-")))

(defun eds/start-blog-post (project title)
  "Create a new post with today's date and a new branch"
  (let* ((title-name (eds/process-title title))
         (branch (concat (format-time-string "%Y-%m-%d") "-" title-name))
         (filename (concat project "/content/md/posts/" branch ".md")))
    (find-file filename)
    (eds/insert-skeleton-blog-post title)
    (save-buffer)
    (magit-branch-create branch "main")
    (magit-checkout branch)))

(defun eds/start-personal-blog-post (title)
  "Create a new post on my personal blog."
  (interactive "sTitle: ")
  (eds/start-blog-post "~/git/eamonnsullivan.co.uk" title))

(defun eds/start-svp-blog-post (title)
  "Create a new post on the svp blog."
  (interactive "sTitle: ")
  (eds/start-blog-post "~/git/svpsouthruislip.org.uk" title))

(defun eds/get-org-directory ()
  "The location of my org directory varies by computer."
  (file-truename "~/Dropbox/org"))

(defun eds/filter-for-regex (regex strings)
  "Filter STRINGS (a list of strings) to those matching REGEX."
  (seq-filter (lambda (str) (string-match-p regex str)) strings))

(defun eds/get-conflicted-org-files ()
  (eds/filter-for-regex "conflicted" (directory-files (eds/get-org-directory))))

(defun eds/dashboard-custom-conflicted-files (list-size)
  (if (> (length (eds/get-conflicted-org-files)) 0)
      (progn
        (dashboard-insert-heading "Dropbox:")
        (insert " There are some dropbox errors in our org files.")
        (insert "\n")
        (dolist (str (eds/get-conflicted-org-files))
          (insert str)))
    (progn
      (dashboard-insert-heading "Dropbox:")
      (insert " No dropbox conficts found in our org files."))
    ))

(defun eds/link-to-svp-contact-page (selected-text)
  "Make a link to the SVP's contact page from SELECTED-TEXT."
  (if (> (length selected-text) 0)
      (format "[%s](../../pages-output/contact/)" selected-text)
    nil))

(defun eds/make-svp-contact-link ()
  "Make a link to the SVP's contact page from the current selection."
  (interactive)
  (if (use-region-p)
      (let ((yank-text (buffer-substring-no-properties (region-beginning) (region-end))))
        (if (> (length yank-text) 0)
            (progn
              (delete-region (region-beginning) (region-end))
              (insert (eds/link-to-svp-contact-page yank-text)))
          (message "No active selection found!")))))


(defun eds/make-org-link (url description)
  "Create an org-roam link with the given URL and DESCRIPTION."
  (format "[[%s][%s]]" url  description))

(defun eds/get-link-line (link)
  "Get an org top-level heading with the provided LINK."
  (format "* %s\n" link))

(defun eds/get-title-line (title)
  "Get the org-roam title line for TITLE."
  (format "#+title: %s\n" title))

(defun eds/get-time-string ()
  "Get the current time as a string formatted for org-roam filenames."
  (format-time-string "%Y%m%dT%H%M%S"))

(defun eds/get-startup-line ()
  "Get the org-roam startup line."
  "#+startup: content\n")

(defun eds/get-subject-from-msg (msg)
  "Return the subject of MSG as a string, or \"No Subject\" if there isn't a subject."
  (or (plist-get msg :subject) "No Subject"))

(defun eds/get-link-from-link (link)
  "Extract the URL from an org-roam LINK."
  (if (string-match "\\[\\[\\(.*?\\)\\]\\[.*?\\]\\]" link)
      (match-string 1 link)
    nil))

(defun eds/get-org-file-name (title)
  "Get a suitable org-roam filename for TITLE."
  (let* ((time-string (eds/get-time-string))
         (extension "org")
         (clean-title (eds/strip-invalid-chars title))
         (slug (eds/process-title clean-title))
         (file-base-name (concat time-string "-" slug "." extension)))
    (expand-file-name file-base-name (eds/get-org-directory))))

(defun eds/ref-link-org-roam (title link &optional category todo)
  "Create or update an org-roam node with the given LINK and TITLE.
   If CATEGORY is provided, set the CATEGORY property. If TODO is provided,
   create a TODO heading."
  (let* ((file-name (eds/get-org-file-name title))
         (title-line (eds/get-title-line title))
         (startup-line (eds/get-startup-line))
         (link-line (eds/get-link-line link))
         (agendatag "#+filetags: :agenda:\n"))
    (find-file file-name)
    ;; In the new buffer
    (if todo
        (progn
          (insert (concat title-line startup-line agendatag))
          (insert "\n\n* TODO ")
          (org-agenda-file-to-front))
      (insert (concat title-line startup-line link-line)))
    (org-id-get-create)
    (org-set-property "ROAM_REFS" (or (eds/get-link-from-link link) link))
    (if category
        (org-set-property "CATEGORY" category))
    (end-of-buffer)
    (save-buffer)))

(defun eds/create-new-note-from-clipboard-link (title)
  "Create or update an org roam node from a (presumable) url in the clipboard."
  (interactive "sTitle: ")
  (let* ((clipboard-content (or (gui-get-selection 'CLIPBOARD) "Clipboard is empty."))
         (link (eds/make-org-link clipboard-content title)))
    (eds/ref-link-org-roam title link)))

(defun eds/orgify-msg (msg)
  "Create a new org-roam node from an email message."
  (let* ((link (org-store-link msg nil))
         (subject (eds/get-subject-from-msg msg)))
    (eds/ref-link-org-roam subject link)))

(defun eds/create-todo-from-email (msg)
  "Create a new org-roam TODO from an email message MSG."
  (let* ((link (org-store-link msg nil))
         (subject (eds/get-subject-from-msg msg)))
    (eds/ref-link-org-roam subject link "Emails" t)))

(defun eds/get-org-agenda-files ()
  "Return a list of org files containing the :agenda: tag, using grep and shell glob expansion."
  (let* ((org-directory (eds/get-org-directory))
         (default-directory (concat org-directory "/"))
         (cmd "grep -l ':agenda:' *.org")
         (output (shell-command-to-string cmd)))
    (mapcar (lambda (file) (expand-file-name file org-directory))
            (split-string output "\n" t))))

(defun eds/get-from-field (msg)
  "Return the 'from' field of MSG as a string."
  (plist-get msg :from))

(defun eds/get-contact-email (contact)
  (plist-get contact :email))

(defun eds/get-mu4e-from-search-string (msg)
  "Return a mu4e search string for the 'from' field of MSG."
  (let* ((from-field (eds/get-from-field msg))
         (contact (car from-field))
         (email (eds/get-contact-email contact)))
    (concat "from:" email)))

(defun eds/get-sendmail-extra-args (from-email)
  (let ((account
         (cond
          ((string-match "eamonn.sullivan@gmail.com" from-email) "gmail-eamonn")
          ((string-match "svpsouthruislip@gmail.com" from-email) "gmail-svp")
          ((string-match "eamonnsullivan.co.uk" from-email) "fastmail"))))
    (if account
        (list "-a" account)
      nil)))

  (provide 'eds)
