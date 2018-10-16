;;; -*- lexical-binding: t -*-
;;; eds.el --- Library for my own tweaks to various packages

(require 'eds-test)

(defun eds/insert-enzyme-test-case (arg)
  "Insert a skeleton test case at point. If a prefix is used, make it synchronous."
  (interactive "P")
  (move-beginning-of-line 1)
  (let ((description (read-string "Test description: "))
        (function-signature (if arg "()" "async ()")))
    (insert (format "test(\"%s\", %s => {\n\n});" description function-signature))
    (forward-line -1)
    (indent-for-tab-command)))

(defun eds/insert-skeleton-test-file (export module-name)
  "Insert a basic skeleton for a test file."
  (goto-char (point-min))
  (insert "import React from 'react';\n")
  (insert "import { shallow } from 'enzyme';\n")
  (insert (format "import %s from './%s';\n\n" export module-name))
  (insert "test('', async () => {\n\n});\n")
  (forward-line -2)
  (indent-for-tab-command))

(defun eds/find-default-export ()
  "Try to find the default export for the current module."
  (save-excursion
    (goto-char (point-min))
    (let ((pos (re-search-forward "^export default " nil t)))
      (if pos
          (let ((default-export (string-trim (buffer-substring-no-properties pos (point-max)))))
            (if (string-match ";$" default-export)
                (substring default-export 0 -1)
              default-export))
        "{ NoDefaultExportFound }"))))

(defun eds/create-or-open-enzyme-test-file (test-file)
  "Open the current buffer's test file or create one if none is found."
  (if (file-exists-p test-file)
      (find-file test-file)
    (let* ((module-name (file-name-nondirectory (substring test-file 0 (string-match ".test.js" test-file))))
           (default-export (eds/find-default-export)))
      (find-file test-file)
      (rjsx-mode)
      (eds/insert-skeleton-test-file default-export module-name))))

(defun eds/is-js (fn)
  "Is the file javascript?"
  (and fn (equal (downcase (file-name-extension fn)) "js")))

(defmacro eds/when-file-is-js (fn foo)
  "Do something if filename is javascript."
  `(if (and ,fn (equal (downcase (file-name-extension ,fn)) "js")) ,foo))

(defmacro eds/if-js-test (fn then else)
  "Do something if filename is a javascript test file or something else if it isn't."
  (declare (indent 2))
  `(eds/when-file-is-js ,fn (if (string-match ".test.js$" ,fn) ,then ,else)))

(defun eds/get-test-or-impl (fn)
  "Get this file's corresponding test or implementation filename."
  (if (eds/is-js fn)
      (let* ((basename (substring fn 0 (string-match (eds/if-js-test fn ".test.js$" ".js$") fn)))
             (ext-to-add (eds/if-js-test fn ".js" ".test.js")))
        (concat basename ext-to-add))
    fn))

(defun eds/toggle-test-implementation ()
  "Toggle between the test and implementation file of a javascript JSX module.
When trying to open the test file, create a new test file if we can't find an existing one."
  (interactive)
  (eds/if-js-test (buffer-file-name)
      (let ((implementation-file (eds/get-test-or-impl buffer-file-name)))
        (find-file implementation-file))
    (let ((test-file (eds/get-test-or-impl buffer-file-name)))
      (eds/create-or-open-enzyme-test-file test-file))))

(defun eds/extract-jira-ticket (input)
  (if (string-match "[A-Za-z]+-[0-9]+" input)
      (upcase (match-string 0 input))
    "NO-TICKET"))

(defun eds/insert-git-branch-name (branch)
  "Insert the current git branch name at point, surrounded by square brackets.
We use this to make the jira tickets easy to spot in the commit messages."
  (interactive)
  (insert "[" (eds/extract-jira-ticket branch) "] "))

(defun eds/kill-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-region (point) (progn (forward-word arg) (point)))))

(defun eds/backward-kill-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (eds/kill-word (- arg)))

(defun eds/switch-to-previous-buffer ()
    "Switch to previously open buffer.
  Repeated invocations toggle between the two most recently open buffers."
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun eds/open-buffer-on-desktop()
  "Open the current directory in Mac OS X finder, Nautilus or Explorer"
  (interactive)
  (let ((file (buffer-file-name))
        (cmd (cond ((eq system-type 'darwin) "open")
                   ((eq system-type 'gnu/linux) "nautilus")
                   ((eq system-type 'windows-nt) "explorer") ;; haven't tested this
                   (t nil))))
    (if (and file cmd)
        (shell-command
         (format "%s %s" (executable-find cmd) (file-name-directory file)))
      (display-warning :error "Buffer not attached to any file or unknown system!"))))

(provide 'eds)
