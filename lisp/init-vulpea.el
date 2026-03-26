;;; init-vulpea.el --- Vulpea configuration initialization -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan <me@eamonnsullivan.co.uk>

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Maintainer: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Created: 2026-03-05
;; Package-Requires: ((emacs "31"))
;; Keywords: notes, org, convenience
;; URL: https://github.com/eamonnsullivan/emacs.d

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides initialization and configuration for Vulpea,
;; a package for managing notes in Org mode.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(require 'eds-org)
(require 'init-org)

(use-package vulpea-ui
  :straight t)
  ;; :config
  ;; workaround for https://github.com/d12frosted/vulpea-ui/issues/21
  ;; (vulpea-ui-unregister-widget 'outline))

(use-package vulpea
  :straight t
  :hook
  (after-init . eds-org/update-agenda-files)
  :config
  (setopt vulpea-db-sync-directories (list (eds-org/get-org-directory))
          vulpea-buffer-alias-property "ROAM_ALIASES"
          vulpea-db-sync-scan-on-enable 'async
          vulpea-db-sync-external-method 'auto
          vulpea-db-sync-scan-on-enable 'async
          vulpea-ui-fast-parse t
          vulpea-create-default-function
          (lambda (title)
            (list :file-name (format "%s_%s.org"
                                     (format-time-string "%Y%m%d%H%M%S")
                                     (vulpea-title-to-slug title))
                  :tags (when (string-match-p "meeting" title)
                            '("meeting"))
                  :properties (if (string-match-p "meeting" title)
                                  (list (cons "CREATED" (format-time-string "%FT%T%z"))
                                        (cons "CATEGORY" "Meeting"))
                                (list (cons "CREATED" (format-time-string "%FT%T%z"))
                                      (cons "CATEGORY" "Note")))
                  :body (if (string-match-p "meeting" title)
                            "\n\n* Actions\n* Notes\n%u\n- [[id:6D43870C-DBA0-4E2D-88D9-3D25BB693FD9][meetings]]\n"
                          "\n"))))
  (vulpea-db-autosync-mode +1)
  (add-to-list 'org-capture-templates
               '("m" "meeting" entry
                 (file (lambda ()
                         (vulpea-note-path
                          (vulpea-create
                           "Meeting notes"
                           nil
                           :tags '("meetings")
                           :properties (list (cons "CREATED" (format-time-string "%FT%T%z"))
                                             (cons "CATEGORY" "Meeting"))))))
                 "* Actions\n* Notes\n%u\n- [[id:6D43870C-DBA0-4E2D-88D9-3D25BB693FD9][meetings]]\n%?"))
  (add-hook 'org-mode-hook #'vulpea-title-change-detection-mode)
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
         ("C-c n R" . org-roam-refile)
         ("C-c n t" . eds-org/update-agenda-files) ; update list of agenda files, manually
         ;; Dailies
         ("C-c n j" . vulpea-journal)))

(use-package vulpea-journal
  :after (vulpea vulpea-ui)
  :config
  (vulpea-journal-setup)
  (setq vulpea-journal-default-template
      (vulpea-journal-template-daily
       :file-name "journal/%Y-%m-%d.org"
       :title "%A, %B %d, %Y"
       :tags '("journal")
       :body "\n\n* Gratitude practice\n")))


(provide 'init-vulpea)

;;; init-vulpea.el ends here
