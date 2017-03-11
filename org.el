;; org mode stuff

(defvar org-directory)
(defvar org-default-notes-file)
(defvar org-capture-templates)
(setq org-directory "~/git/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline (concat org-directory "/notes.org") "Tasks"))
        ("i" "Ideas" entry (file+headline (concat org-directory "/notes.org") "Ideas"))
        ("n" "Notes" entry (file+headline (concat org-directory "/notes.org") "General Notes"))))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
