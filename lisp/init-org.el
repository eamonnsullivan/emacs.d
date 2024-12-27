;;; -*- lexical-binding: t -*-
;;; init-org.el --- org mode stuff


(use-package ob-typescript)
(use-package ob-go)
(straight-use-package
 '(edraw-org :type git :host github :repo "misohena/el-easydraw"))

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
        org-agenda-start-on-weekday nil
        org-agenda-dim-blocked-tasks nil
        org-agenda-inhibit-startup t
        org-agenda-ignore-drawer-properties '(effort appt)
        org-fold-catch-invisible-edits t
        org-capture-templates `(("t" "Todo" entry (file eds-org-index-file)
                                 "* TODO %?\n SCHEDULED: %t\n %a")
                                ("w" "Work note" entry (file org-default-notes-file)
                                 "* %?\n %U\n %a")
                                ("p" "Personal note" entry (file eds-org-personal-file)
                                 "* %?\n %U\n %a"))
        org-todo-keywords '((sequence "TODO(t)" "IN_PROGRESS(p)" "|" "DONE(d)" "SKIPPED(k)" "CANCELLED(c)"))
        org-agenda-custom-commands '(("ha" tags-todo "homeautomation|home-it")
                                     ("hg" tags-todo "gardening")
                                     ("pt" tags-todo "personal")
                                     ("ps" tags-todo "shopping")
                                     ("ww" tags-todo "work")))
  (add-to-list 'org-modules 'org-timer)
  (add-hook 'org-clock-in-hook (lambda ()
      (if (not org-timer-countdown-timer)
          (org-timer-set-timer '(16))))))

(with-eval-after-load 'org
  (require 'edraw-org)
  (edraw-org-setup-default))

(with-eval-after-load "ox"
  (require 'edraw-org)
  (edraw-org-setup-exporter))

(use-package org-roam
  :straight (:host github :repo "org-roam/org-roam"
             :files (:defaults "extensions/*"))
  :config
  (define-key org-roam-mode-map [mouse-1] #'org-roam-visit-thing)
  (setq org-roam-directory (eds/get-org-directory)
        org-roam-database-connector 'sqlite-builtin
        org-roam-db-gc-threshold most-positive-fixnum
        org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag))
        org-roam-capture-templates
        `(("d" "default" entry nil
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
           :unnarrowed t)
          ("m" "meeting" entry "* Notes\n%u\n%?"
           :target
           (file+head
            "%<%Y%m%d%H%M%S>-${slug}.org"
            ,(concat "#+title: ${title}"
                     "\n#+filetags: :meeting:"
                     "\n- [[id:6D43870C-DBA0-4E2D-88D9-3D25BB693FD9][meetings]]"
                     "\n* Actions\n"))
           :unnarrowed t)
          ("t" "todo" entry "* TODO %?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
           :unnarrowed t)
          ("x" "training" entry "* Notes\n%u\n%?"
           :target
           (file+head
            "%<%Y%m%d%H%M%S>-${slug}.org"
            ,(concat "#+title: ${title}"
                     "\n#+filetags: :training:"
                     "\n- [[id:C9EB836D-F0DD-47CE-B8F3-B44FD1A0A0E6][training]]\n"))
           :unnarrowed t)
          ("i" "idea" entry "* %?\n%u\n"
           :target (file+head
                    "%<%Y%m%d%H%M%S>-${slug}.org"
                    ,(concat "#+title: ${title}"
                             "\n#+filetags: :ideas:"
                             "\n- [[id:A5284C15-BADD-4A2D-8299-6A8A24339000][Ideas]]\n")))))
  (org-roam-db-autosync-mode)
  (define-key org-roam-mode-map [mouse-1] #'org-roam-preview-visit)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n a" . org-roam-tag-add)
         ("C-c n o" . eds/switch-to-org-roam-buffer)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today)))

(defun eds/switch-to-org-roam-buffer ()
  (interactive)
  (switch-to-buffer-other-frame "*org-roam*"))

;; for org-roam-buffer-toggle
;; Use side-window like V1
;; This can take advantage of slots available with it
(add-to-list 'display-buffer-alist
    '("\\*org-roam\\*"
        (display-buffer-in-side-window)
        (side . right)
        (slot . 0)
        (window-width . 0.25)
        (preserve-size . (t . nil))
        (window-parameters . ((no-other-window . t)
                              (no-delete-other-windows . t)))))

(provide 'init-org)

;;; org.el ends here
