;;; -*- lexical-binding: t -*-
;;; init-org.el --- org mode stuff


(use-package ob-typescript)
(use-package ob-go)
(straight-use-package
 '(edraw-org :type git :host github :repo "misohena/el-easydraw"))
(use-package ob-http)

(require 'eds)

(use-package org-modern
  :after org
  :config
  (with-eval-after-load 'org (global-org-modern-mode)))

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
     (python . t)
     (dot . t)
     (plantuml . t)
     (http . t)))
  (setq org-directory (eds/get-org-directory)
        org-default-notes-file (concat org-directory "/notes.org")
        eds-org-index-file (concat org-directory "/index.org")
        eds-org-personal-file (concat org-directory "/personal.org")
        eds-org-calendar-file (concat org-directory "/calendar.org")
        org-agenda-files (eds/get-org-agenda-files)
	org-refile-targets '((org-agenda-files :maxlevel . 5))
        org-src-fontify-natively t
        org-log-into-drawer t
        org-hide-emphasis-markers t
        org-agenda-include-diary t
        org-timer-default-timer 25
        org-plantuml-executable-path "/opt/homebrew/bin/plantuml"
        org-plantuml-exec-mode 'plantuml
        org-capture-use-agenda-date t
        org-confirm-babel-evaluate nil
        org-agenda-start-on-weekday nil
        org-agenda-dim-blocked-tasks nil
        org-agenda-inhibit-startup t
        org-agenda-ignore-drawer-properties '(effort appt)
        org-agenda-skip-scheduled-repeats-after-deadline t
        org-agenda-skip-deadline-prewarning-if-scheduled t
        org-fold-catch-invisible-edits t
        org-startup-folded 'content
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
  (add-to-list 'org-agenda-files eds-org-calendar-file)
  (add-hook 'org-clock-in-hook (lambda ()
                                 (if (not org-timer-countdown-timer)
                                     (org-timer-set-timer '(16)))))
  (add-to-list 'org-modules 'org-habit)
  (add-hook 'auto-save-hook 'org-save-all-org-buffers))

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
  (setq org-roam-mode-sections
        (list #'org-roam-backlinks-section
              #'org-roam-reflinks-section
              #'org-roam-unlinked-references-section
              ))

  (define-key org-roam-mode-map [mouse-1] #'org-roam-visit-thing)
  (setq org-roam-directory (eds/get-org-directory)
        org-roam-database-connector 'sqlite-builtin
        org-roam-graph-executable "dot"
        org-roam-db-gc-threshold most-positive-fixnum
        org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag))
        org-roam-capture-templates
        `(("d" "default" entry "* ${title}\n%?"
           :target (file+head
                    "%<%Y%m%d%H%M%S>-${slug}.org"
                    ,(concat "#+title: ${title}"
                             "\n#+startup: content"))
           :unnarrowed t)
          ("m" "meeting" entry "* ${title} Notes\n%u\n%?"
           :target
           (file+head
            "%<%Y%m%d%H%M%S>-${slug}.org"
            ,(concat "#+title: ${title}"
                     "\n#+filetags: :meeting:"
                     "\n#+startup: content"
                     "\n- [[id:6D43870C-DBA0-4E2D-88D9-3D25BB693FD9][meetings]]"
                     "\n* Actions\n"))
           :unnarrowed t)
          ("t" "todo" entry "* TODO %?"
           :target (file+head
                    "%<%Y%m%d%H%M%S>-${slug}.org"
                    ,(concat "#+title: ${title}"
                             "\n#+startup: content"))
           :unnarrowed t)
          ("x" "training" entry "* ${title} Notes\n%u\n%?"
           :target
           (file+head
            "%<%Y%m%d%H%M%S>-${slug}.org"
            ,(concat "#+title: ${title}"
                     "\n#+filetags: :training:"
                     "\n#+startup: content"
                     "\n- [[id:C9EB836D-F0DD-47CE-B8F3-B44FD1A0A0E6][training]]\n"))
           :unnarrowed t)
          ("i" "idea" entry "* %?\n%u\n"
           :target (file+head
                    "%<%Y%m%d%H%M%S>-${slug}.org"
                    ,(concat "#+title: ${title}"
                             "\n#+filetags: :ideas:"
                             "\n#+startup: content"
                             "\n- [[id:A5284C15-BADD-4A2D-8299-6A8A24339000][Ideas]]\n"))))
        org-roam-capture-ref-templates
        '(("r" "ref" entry "* ${title}\n${body}\n%?"
           :target
           (file+head
            "%<%Y%m%d%H%M%S>-${slug}.org"
            "#+title: ${title}")
           :unnarrowed t)))

  (when (eq system-type 'darwin)
    (setq org-roam-graph-viewer "/Applications/Firefox.app/Contents/MacOS/firefox"))

  (defun eds/org-roam-graph-small ()
    (interactive)
    (org-roam-graph 2 (org-roam-node-at-point)))

  (org-roam-db-autosync-mode)
  (define-key org-roam-mode-map [mouse-1] #'org-roam-preview-visit)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . eds/org-roam-graph-small)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n a" . org-roam-tag-add)
         ("C-c n r" . eds/create-new-note-from-clipboard-link)
         ("C-c n n" . org-id-get-create) ; useful for making a heading a node
         ("C-c n A" . org-roam-alias-add) ; add an alias to the current node
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today)))

(require 'org-roam-protocol)

(use-package org-mem
  :defer
  :config
  (setq org-mem-do-sync-with-org-id t) ;; Optional
  (setq org-mem-watch-dirs
        (list (eds/get-org-directory))) ;; Your org-roam-directory here
  (org-mem-updater-mode))

(use-package org-node
  :init
  ;; Optional key bindings
  (keymap-set global-map "M-o" org-node-global-prefix-map)
  (with-eval-after-load 'org
    (keymap-set org-mode-map "M-o" org-node-org-prefix-map))
  :config
  (setq org-node-creation-fn #'org-node-new-via-roam-capture
        org-node-file-slug-fn #'org-node-slugify-like-roam-default
        org-node-file-timestamp-format "%Y%m%d%H%M%S-"
        org-node-backlink-do-drawers t
        org-node-alter-candidates t
        org-node-display-sort-fn #'org-node-sort-by-mtime-property)
  (org-node-cache-mode)
  (org-node-roam-accelerator-mode)
  (org-node-backlink-mode)
  (setq org-node-display-sort-fn
      #'org-node-sort-by-mtime-property))

(provide 'init-org)

;;; org.el ends here
