;;; -*- lexical-binding: t -*-
;;; init-org.el --- org mode stuff


(use-package ob-typescript)
(use-package ob-go)

(require 'eds)

;; (use-package '(org-plus-contrib :includes org))

(use-package org
  :init
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'variable-pitch-mode)
  (add-to-list 'ispell-skip-region-alist '("^#+begin_src" . "^#+end_src"))
  :diminish visual-line-mode
  :diminish org-indent-mode
  :config
  (require 'ox-latex)
  (require 'ob-clojure)
  (require 'ob-typescript)
  (require 'ob-go)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (scheme . t)
     (js . t)
     (typescript . t)
     (clojure . t)
     (haskell . t)
     (python . t)))
  (setq org-directory (eds/get-org-directory)
        org-default-notes-file (concat org-directory "/notes.org")
        eds-org-index-file (concat org-directory "/index.org")
        eds-org-personal-file (concat org-directory "/personal.org")
        org-agenda-files (backquote
                          (,(concat org-directory "/")))
	org-refile-targets '((org-agenda-files :maxlevel . 5))
        org-src-fontify-natively t
        org-hide-emphasis-markers t
        org-agenda-include-diary t
        org-timer-default-timer 25
        org-capture-use-agenda-date t
        org-confirm-babel-evaluate nil
        org-capture-templates `(("t" "Todo" entry (file eds-org-index-file)
                                 "* TODO %?\n SCHEDULED: %t\n %a")
                                ("w" "Work note" entry (file org-default-notes-file)
                                 "* %?\n %U\n %a")
                                ("p" "Personal note" entry (file eds-org-personal-file)
                                 "* %?\n %U\n %a"))
        org-todo-keywords '((sequence "TODO(t)" "IN_PROGRESS(p)" "|" "DONE(d)" "SKIPPED(k)" "CANCELLED(c)")))
  (add-to-list 'org-modules 'org-timer)
  (add-hook 'org-clock-in-hook (lambda ()
      (if (not org-timer-countdown-timer)
          (org-timer-set-timer '(16))))))

(use-package org-roam
  :straight (:host github :repo "org-roam/org-roam"
             :files (:defaults "extensions/*"))
  :config
  (setq org-roam-directory (eds/get-org-directory)
        org-roam-database-connector 'sqlite-builtin
        org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag))
        org-roam-graph-executable "/usr/bin/neato")
  (org-roam-db-autosync-mode)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today)))

(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.33)
               (window-height . fit-window-to-buffer)))

(provide 'init-org)

;;; org.el ends here
