;; org mode stuff

(defvar org-directory)
(defvar org-default-notes-file)
(defvar org-capture-templates)
(defvar org-mobile-directory)
(defvar org-mobile-inbox-for-pull)
(defvar org-mobile-files)

(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline (concat org-directory "/tasks.org") "Tasks"))
        ("i" "Ideas" entry (file+headline (concat org-directory "/ideas.org") "Ideas"))
        ("n" "Notes" entry (file+headline (concat org-directory "/notes.org") "General Notes"))))

;; mobile org
(require 'org-mobile)
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
(setq org-mobile-inbox-for-pull (concat org-directory "/inbox.org"))
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
(setq org-mobile-files '(org-directory))
