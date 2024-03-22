;;; -*- lexical-binding: t -*-
;;; init-org.el --- org mode stuff


(use-package ob-typescript)

(require 'eds)

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
  (setq org-directory (eds/get-org-directory))
  (setq org-default-notes-file (concat org-directory "/notes.org")
        org-capture-templates
        `(("t" "Todo" entry (file+headline ,(concat org-directory "/tasks.org") "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("i" "Ideas" entry (file+headline ,(concat org-directory "/ideas.org") "Ideas"))
          ("n" "Notes" entry (file+headline ,(concat org-directory "/notes.org") "General Notes"))
          ("p" "Personal Todo" entry (file+headline ,(concat org-directory "/personal.org") "Personal Tasks")
           "* TODO %?\n  %i\n  %a"))
        org-agenda-files (backquote
                          (,(concat org-directory "/personal.org")
                           ,(concat org-directory "/notes.org")
                           ,(concat org-directory "/tasks.org")
                           ,(concat org-directory "/")))
        org-src-fontify-natively t
        org-hide-emphasis-markers t))

(use-package org-roam
  :config
  (setq org-roam-directory (eds/get-org-directory))
  (org-roam-db-autosync-mode)
  :bind (("\C-c c" . org-roam-capture)
         ("\C-c a" . org-roam-tag-add)
         ("\C-c i" . org-roam-node-insert))
)

(provide 'init-org)

;;; org.el ends here
