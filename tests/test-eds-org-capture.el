;;; test-eds-org-capture.el --- Capture lifecycle tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Exercise registered policy through real Org, Org-roam and Vulpea captures.

;;; Code:

(load-file "tests/setup.el")
(require 'cl-lib)
(require 'org-roam)
(require 'org-roam-protocol)
(require 'vulpea)
(require 'eds-org-capture)

(defun eds-test-with-capture-directory (test)
  "Call TEST with a temporary notes directory and isolated databases."
  (let* ((directory (make-temp-file "eds-capture-" t))
         (default-directory directory)
         (org-directory directory)
         (org-roam-directory directory)
         (org-roam-db-location (expand-file-name "roam.db" directory))
         (org-roam-db--connection (make-hash-table :test #'equal))
         (vulpea-default-notes-directory directory)
         (vulpea-db-location (expand-file-name "vulpea.db" directory))
         (vulpea-db--connection nil)
         (vulpea-db-sync-directories (list directory))
         (org-id-method 'ts)
         (org-id-locations nil)
         (org-id-locations-file (expand-file-name "ids" directory))
         (org-mode-hook nil)
         (org-roam-capture-new-node-hook nil)
         (org-capture-before-finalize-hook nil)
         (org-capture-after-finalize-hook nil)
         (org-capture-prepare-finalize-hook nil)
         (org-capture-mode-hook nil)
         (org-stored-links nil)
         (org-store-link-plist nil)
         (org-capture-plist nil)
         (org-capture-current-plist nil)
         (org-capture-last-stored-marker (make-marker))
         (variables '(org-capture-templates org-roam-capture-templates
                      org-roam-capture-ref-templates vulpea-create-default-function))
         (defaults (mapcar (lambda (variable)
                            (cons variable (default-value variable))) variables)))
    (unwind-protect
        (save-window-excursion (funcall test directory))
      (dolist (buffer (buffer-list))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when (and buffer-file-name
                       (file-in-directory-p buffer-file-name directory))
              (set-buffer-modified-p nil)
              (kill-buffer buffer)))))
      (org-roam-db--close-all)
      (vulpea-db-close)
      (dolist (entry defaults) (set-default (car entry) (cdr entry)))
      (delete-directory directory t))))

(describe "eds-org-capture registration"
  (it "registers exact key sets and installs hooks idempotently"
    (eds-test-with-capture-directory
     (lambda (_directory)
       (eds-org-capture-register-org-templates)
       (expect (mapcar #'car org-capture-templates) :to-equal '("t" "w" "p"))
       (eds-org-capture-register-roam-templates)
       (eds-org-capture-register-roam-templates)
       (expect (mapcar #'car org-roam-capture-templates)
               :to-equal '("d" "m" "t" "x" "i"))
       (expect (mapcar #'car org-roam-capture-ref-templates) :to-equal '("r" "T"))
       (expect org-roam-capture-new-node-hook :to-equal '(eds-org/ensure-capture-category))
       (expect org-capture-before-finalize-hook :to-equal '(eds-org-capture-before-finalize))
       (eds-org-capture-register-vulpea-meeting-template)
       (eds-org-capture-register-vulpea-meeting-template)
       (expect (mapcar #'car org-capture-templates) :to-equal '("m" "t" "w" "p"))))))

(describe "eds-org-capture Org-roam lifecycle"
  (it "finalizes a new node with a prompted CATEGORY"
    (eds-test-with-capture-directory
     (lambda (directory)
       (eds-org-capture-register-roam-templates)
       (spy-on 'eds-org/read-category :and-return-value "Research")
       (org-roam-capture- :keys "d" :node (org-roam-node-create :title "Research note"))
       (org-capture-finalize)
       (let ((files (directory-files directory t "\\.org$")))
         (expect (length files) :to-equal 1)
         (with-temp-buffer
           (insert-file-contents (car files))
           (org-mode)
           (goto-char (point-min))
           (expect (eds-org/explicit-category) :to-equal "Research")
           (expect (buffer-string) :to-match (regexp-quote "#+title: Research note"))))
       (expect 'eds-org/read-category :to-have-been-called)))))

(describe "eds-org-capture explicit CATEGORY lifecycle"
  (it "keeps an explicit file CATEGORY without prompting"
    (eds-test-with-capture-directory
     (lambda (directory)
       (eds-org-capture-register-roam-templates)
       (let* ((file (expand-file-name "existing.org" directory))
              (id (org-id-new)))
         (with-temp-file file
           (insert (format ":PROPERTIES:\n:ID: %s\n:CATEGORY: Meeting\n:END:\n#+title: Existing\n" id)))
         (spy-on 'eds-org/read-category :and-return-value "Unexpected")
         (org-roam-capture- :keys "d"
                           :node (org-roam-node-create :id id :file file :title "Existing"))
         (org-capture-finalize)
         (expect 'eds-org/read-category :not :to-have-been-called)
         (with-temp-buffer
           (insert-file-contents file)
           (org-mode)
           (goto-char (point-min))
           (expect (eds-org/explicit-category) :to-equal "Meeting")))))))

(describe "eds-org-capture reference lifecycle"
  (it "stores a reference and applies URL tags during finalization"
    (eds-test-with-capture-directory
     (lambda (directory)
       (eds-org-capture-register-roam-templates)
       (spy-on 'eds-org/read-category :and-return-value "Work")
       (org-roam-protocol-open-ref
        '(:template "r" :title "Design - Confluence"
          :ref "https://bbc.atlassian.net/wiki/design" :body "Selected text"))
       (org-capture-finalize)
       (let ((files (directory-files directory t "\\.org$")))
         (expect (length files) :to-equal 1)
         (with-temp-buffer
           (insert-file-contents (car files))
           (org-mode)
           (expect (buffer-string) :to-match (regexp-quote "#+title: Design\n"))
           (expect (buffer-string) :to-match "Selected text")
           (expect (vulpea-buffer-tags-get) :to-contain "confluence")
           (expect (vulpea-buffer-tags-get) :to-contain "reference")))))))

(describe "eds-org-capture Vulpea lifecycle"
  (it "creates Note and Meeting files with distinct defaults"
    (eds-test-with-capture-directory
     (lambda (_directory)
       (eds-org-capture-register-vulpea-defaults)
       (dolist (title '("Ordinary note" "Project meeting"))
         (let* ((meeting (string-match-p "meeting" title))
                (note (vulpea-create title)))
           (expect (file-exists-p (vulpea-note-path note)) :to-be-truthy)
           (with-temp-buffer
             (insert-file-contents (vulpea-note-path note))
             (org-mode)
             (goto-char (point-min))
             (expect (eds-org/explicit-category) :to-equal (if meeting "Meeting" "Note"))
             (expect (vulpea-buffer-tags-get) :to-equal (if meeting '("meeting") nil))
             (when meeting (expect (buffer-string) :to-match "\\* Actions")))))))))

(describe "eds-org-capture Vulpea meeting capture"
  (it "finalizes Org's meeting template with explicit CATEGORY and consistent meeting tag"
    (eds-test-with-capture-directory
     (lambda (directory)
       (eds-org-capture-register-org-templates)
       (eds-org-capture-register-roam-templates)
       (eds-org-capture-register-vulpea-defaults)
       (eds-org-capture-register-vulpea-meeting-template)
       (spy-on 'eds-org/read-category :and-return-value "Unexpected")
       (org-capture nil "m")
       (org-capture-finalize)
       (expect 'eds-org/read-category :not :to-have-been-called)
       (let ((files (directory-files directory t "\\.org$")))
         (expect (length files) :to-equal 1)
         (with-temp-buffer
           (insert-file-contents (car files))
           (org-mode)
           (goto-char (point-min))
           (expect (eds-org/explicit-category) :to-equal "Meeting")
           (expect (vulpea-buffer-tags-get) :to-equal '("meeting"))
           (expect (buffer-string) :to-match "\\* Actions")))))))

(provide 'test-eds-org-capture)
;;; test-eds-org-capture.el ends here
