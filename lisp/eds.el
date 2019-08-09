;;; -*- lexical-binding: t -*-
;;; eds.el --- Library for my own tweaks to various packages

(require 'eds-test)
(require 'lsp-mode)

(defun eds/insert-enzyme-test-case (arg)
  "Insert a skeleton test case at point. If a prefix is used, make it synchronous."
  (interactive "P")
  (move-beginning-of-line 1)
  (let ((description (read-string
                      "Test description: "))
        (function-signature (if arg "()" "async ()")))
    (insert
     (format
      "test(\"%s\", %s => {\n\n});"
      description
      function-signature))
    (forward-line -1)
    (indent-for-tab-command)))

(defun eds/insert-skeleton-test-file (export module-name)
  "Insert a basic skeleton for a test file."
  (goto-char (point-min))
  (insert
   "import React from 'react';\n")
  (insert
   "import { shallow } from 'enzyme';
")
  (insert
   (format
    "import %s from './%s';\n\n"
    export
    module-name))
  (insert
   "test('', async () => {

});
")
  (forward-line -2)
  (indent-for-tab-command))

(defun eds/find-default-export ()
  "Try to find the default export for the current module."
  (save-excursion
    (goto-char (point-min))
    (let ((pos (re-search-forward
                "^export default "
                nil
                t)))
      (if pos
          (let ((default-export (string-trim
                                 (buffer-substring-no-properties
                                  pos
                                  (point-max)))))
            (if (string-match
                 ";$"
                 default-export)
                (substring default-export 0 -1)
              default-export))
        "{ NoDefaultExportFound }"))))

(defun eds/create-or-open-enzyme-test-file (test-file)
  "Open the current buffer's test file or create one if none is found."
  (if (file-exists-p test-file)
      (find-file test-file)
    (let* ((module-name (file-name-nondirectory
                         (substring
                          test-file
                          0
                          (string-match
                           ".test.js"
                           test-file))))
           (default-export (eds/find-default-export)))
      (find-file test-file)
      (rjsx-mode)
      (eds/insert-skeleton-test-file
       default-export
       module-name))))

(defun eds/is-js (fn)
  "Is the file javascript?"
  (and fn
       (equal (downcase
               (file-name-extension fn))
              "js")))

(defmacro eds/when-file-is-js (fn foo)
  "Do something if filename is javascript."
  `(if
       (and ,fn
            (equal (downcase
                    (file-name-extension ,fn))
                   "js"))
       ,foo))

(defmacro eds/if-js-test (fn then else)
  "Do something if filename is a javascript test file or something else if it isn't."
  (declare (indent 2))
  `(eds/when-file-is-js
    ,fn
    (if (string-match ".test.js$" ,fn)
        ,then
      ,else)))

(defun eds/get-test-or-impl (fn)
  "Get this file's corresponding test or implementation filename."
  (if (eds/is-js fn)
      (let* ((basename (substring
                        fn
                        0
                        (string-match
                         (eds/if-js-test
                             fn
                             ".test.js$"
                           ".js$")
                         fn)))
             (ext-to-add (eds/if-js-test
                             fn
                             ".js"
                           ".test.js")))
        (concat basename ext-to-add))
    fn))

(defun eds/toggle-test-implementation ()
  "Toggle between the test and implementation file of a javascript JSX module.
When trying to open the test file, create a new test file if we can't find an existing one."
  (interactive)
  (eds/if-js-test
      (buffer-file-name)
      (let ((implementation-file (eds/get-test-or-impl
                                  buffer-file-name)))
        (find-file implementation-file))
    (let ((test-file (eds/get-test-or-impl
                      buffer-file-name)))
      (eds/create-or-open-enzyme-test-file
       test-file))))

(defun eds/extract-jira-ticket (input)
  (if (string-match
       "[A-Za-z]+-[0-9]+"
       input)
      (upcase (match-string 0 input))
    "NO-TICKET"))

(defun eds/insert-git-branch-name (branch)
  "Insert the current git branch name at point, surrounded by square brackets.
We use this to make the jira tickets easy to spot in the commit messages."
  (interactive)
  (insert
   "["
   (eds/extract-jira-ticket
    branch)
   "] "))

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
       (point)))))(defun eds/backward-kill-word (arg)
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

(defun eds/trim-lsp-hover-markdown (markdown)
  "Strip out the markdown from an lsp hover string"
  (let* ((strip-quotes (replace-regexp-in-string "```" "" markdown))
         (strip-lang (string-trim (replace-regexp-in-string (lsp-buffer-language) "" strip-quotes))))
    strip-lang))

(defun eds/make-hover-request (doc line character)
  "Make a request to the LSP server for the hover string for this file and location."
  (lsp--make-request
   "textDocument/hover"
   (list :textDocument doc
         :position (lsp--position line character))))

(defun eds/split-hover-string (hover)
  "Split the hover string we get back into the symbol part and the type part."
  (let* ((last-colon (- (length hover) (string-match "\:" (reverse hover))))
         (symbol (string-trim (substring hover 0 (1- last-colon))))
         (type (string-trim (substring hover last-colon))))
    (list symbol type)))

(defun eds/get-symbol-and-type-of-thing-at-point ()
  "Using lsp, if available, find the whole symbol name and the type of the thing at point."
  (interactive)
  (when (and buffer-file-name (lsp--capability "hoverProvider"))
    (let* ((line-widen (save-restriction (widen) (line-number-at-pos)))
           (bol (line-beginning-position))
           (doc-id (lsp--text-document-identifier))
           (response (lsp--send-request
                      (eds/make-hover-request doc-id (1- line-widen) (- (point) bol)))))
      (when response
        (let* ((content (thread-first (gethash "contents" response)))
               (value (gethash "value" content)))
          (when content
            (let* ((trimmed (eds/trim-lsp-hover-markdown value))
                   (split (eds/split-hover-string trimmed))
                   (type (car (cdr split)))
                   (symbol (car split)))
              (list symbol type))))))))

(defun eds/is-assignment-thing-p (line)
  "Returns t if a line appears to be a def, val or var assignment. Nil otherwise."
  (and (string-match-p "=" line) (or (string-match-p "def " line)
                                     (string-match-p "val " line)
                                     (string-match-p "var " line))))

(defun eds/has-annotation-p (line)
  "Returns t if the line appears to already have a type annotation. Nil otherwise."
  (string-match-p "\:[^)]+\=" line))

(defun eds/annotate-scala-symbol-with-type ()
  "Using lsp, if available, append the type of the scala symbol (def, val or var) at point"
  (interactive)
  (when (equal (lsp-buffer-language) "scala")
    (let* ((sym-type (eds/get-symbol-and-type-of-thing-at-point))
          (sym (car sym-type))
          (type (car (cdr sym-type)))
          (line (string-trim (thing-at-point 'line t))))
      (if (and (eds/is-assignment-thing-p line) (not (eds/has-annotation-p line)))
          (let ((sym-with-type (format "%s: %s" sym type)))
            (save-excursion
              (goto-char (point-min))
              (while (search-forward sym nil t)
                (replace-match sym-with-type))))))))

(provide 'eds)
