;;; -*- lexical-binding: t -*-
;;; init-org.el --- org mode stuff


(use-package ob-typescript)

;; (straight-use-package '(org-plus-contrib :includes org))

(use-package org
  :init
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'variable-pitch-mode)
  (add-to-list 'ispell-skip-region-alist '("^#+begin_src" . "^#+end_src"))
  :diminish visual-line-mode
  :diminish org-indent-mode
  :bind (("\C-c a" . org-agenda)
         ("\C-c c" . org-capture)
         ("\C-c l" . org-store-link))
  :config
  (require 'ox-latex)
  (require 'ob-clojure)
  (require 'ob-typescript)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (scheme . t)
     (js . t)
     (typescript . t)
     (clojure . t)
     (python . t)))
  (if (eq system-type 'darwin)
      (setq org-directory (file-truename "~/Sullivan Shared/eamonn-notes/org"))
    (setq org-directory (file-truename "~/sullivan-shared/eamonn-notes/org")))
  (setq org-default-notes-file (concat org-directory "/notes.org")
        org-capture-templates
        `(("t" "Todo" entry (file+headline ,(concat org-directory "/tasks.org") "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("i" "Ideas" entry (file+headline ,(concat org-directory "/ideas.org") "Ideas"))
          ("n" "Notes" entry (file+headline ,(concat org-directory "/notes.org") "General Notes"))
          ("p" "Personal Todo" entry (file+headline ,(concat org-directory "/personal.org") "Personal Tasks")
           "* TODO %?\n  %i\n  %a"))
        org-agenda-files (quote
                          ((concat org-directory "/personal.org")
                           (concat org-directory "/notes.org")
                           (concat org-directory "/tasks.org")))
        org-src-fontify-natively t
        org-hide-emphasis-markers t))

(use-package org-roam
  :config
  (if (eq system-type 'darwin)
      (setq org-roam-directory (file-truename "~/Sullivan Shared/eamonn-notes/org"))
    (setq org-roam-directory (file-truename "~/sullivan-shared/eamonn-notes/org")))
  (org-roam-db-autosync-mode))

(provide 'init-org)

;;; org.el ends here
