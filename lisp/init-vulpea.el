;;; -*- lexical-binding: t -*-
;;; init-vulpea.el

(require 'eds-org)
(require 'init-org)

(use-package vulpea-ui
  :straight t)

(use-package vulpea
  :straight t
  :config
  (setopt vulpea-db-sync-directories (list (eds-org/get-org-directory))
          vulpea-buffer-alias-property "ROAM_ALIASES"
          vulpea-db-sync-scan-on-enable 'async
          vulpea-db-sync-external-method 'auto
          vulpea-create-default-function
          (lambda (title)
            (list :file-name (format "%s_%s.org"
                                     (format-time-string "%Y%m%d%H%M%S")
                                     (vulpea-title-to-slug title))
                  :tags (if (string-match-p "meeting" title)
                            '("meeting")
                          '("note"))
                  :properties (if (string-match-p "meeting" title)
                                  (list (cons "CREATED" (format-time-string "%FT%T%z"))
                                        (cons "CATEGORY" "Meeting"))
                                (list (cons "CREATED" (format-time-string "%FT%T%z"))
                                      (cons "CATEGORY" "Note"))))))
  (vulpea-db-autosync-mode +1)
  (add-to-list 'org-capture-templates
               '("v" "vulpea" entry
                 (file (lambda ()
                         (vulpea-note-path
                          (vulpea-create
                           "Test Vulpea note"
                           nil
                           :tags '("test" "vulpea")
                           :properties (list (cons "CREATED" (format-time-string "%FT%T%z"))
                                             (cons "CATEGORY" "Vulpea Note"))))))
                 "* %?\n%U\n%a\n"))
  :bind (("C-c n l" . vulpea-ui-sidebar-toggle)
         ("C-c n f" . vulpea-find)
         ("C-c n b" . vulpea-find-backlink)
         ("C-c n g" . org-roam-ui-open)
         ("C-c n i" . vulpea-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n a" . vulpea-buffer-tags-add)
         ("C-c n r" . eds-org/create-new-note-from-clipboard-link)
         ("C-c n n" . org-id-get-create) ; useful for making a heading a node
         ("C-c n A" . vulpea-buffer-alias-add) ; add an alias to the current node
         ("C-c n P" . eds-org/set-category-value) ; set the CATEGORY property of the current node
         ("C-c n t" . eds-org/update-agenda-files) ; update list of agenda files, manually
         ;; Dailies
         ("C-c n j" . vulpea-journal)))

(use-package vulpea-journal
  :after (vulpea vulpea-ui)
  :config
  (vulpea-journal-setup))


(provide 'init-vulpea)

;;; init-vulpea.el ends here
