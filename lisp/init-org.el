;;; -*- lexical-binding: t -*-
;;; init-org.el --- org mode stuff


(use-package ob-typescript)

(require 'eds)

;; (straight-use-package '(org-plus-contrib :includes org))

;; example template
;; org-capture-templates
;;         `(("t" "Todo" entry (file+headline ,(concat org-directory "/tasks.org") "Tasks")
;;            "* TODO %?\n  %i\n  %a")
;;           ("i" "Ideas" entry (file+headline ,(concat org-directory "/ideas.org") "Ideas"))
;;           ("n" "Notes" entry (file+headline ,(concat org-directory "/notes.org") "General Notes"))
;;           ("p" "Personal Todo" entry (file+headline ,(concat org-directory "/personal.org") "Personal Tasks")
;;            "* TODO %?\n  %i\n  %a"))

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
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (scheme . t)
     (js . t)
     (typescript . t)
     (clojure . t)
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
        org-capture-templates `(("t" "Todo" entry (file eds-org-index-file)
                                 "* TODO %?\n SCHEDULED: %t\n %a")
                                ("w" "Work note" entry (file org-default-notes-file)
                                 "* %?\n %t\n %a")
                                ("p" "Personal note" entry (file eds-org-personal-file)
                                 "* %?\n %t\n %a")))
  (add-to-list 'org-modules 'org-timer)
  (add-hook 'org-clock-in-hook (lambda ()
      (if (not org-timer-countdown-timer)
          (org-timer-set-timer '(16))))))

(use-package org-roam
  :straight (:host github :repo "org-roam/org-roam"
             :files (:defaults "extensions/*"))
  :config
  (setq org-roam-directory (eds/get-org-directory))
  (org-roam-db-autosync-mode)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.33)
               (window-height . fit-window-to-buffer)))

(provide 'init-org)

;;; org.el ends here
