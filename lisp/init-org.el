;;; init-org.el --- Org mode configuration initialisation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan <me@eamonnsullivan.co.uk>

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Maintainer: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Created: 2017-02-24
;; Version: 0.1
;; Package-Requires: ((emacs "31") (org "9.8"))
;; Keywords: notes, organisation, productivity
;; URL: https://github.com/eamonnsullivan/init-org

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides initialisation and configuration for Org mode,
;; enabling advanced note-taking, organisation, and productivity features.

;;; Licence:

;; This programme is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public Licence as published by
;; the Free Software Foundation, either version 3 of the Licence, or
;; (at your option) any later version.

;; This programme is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public Licence for more details.

;; You should have received a copy of the GNU General Public Licence
;; along with this programme.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(use-package ob-typescript)
(use-package ob-go)
(straight-use-package
 '(edraw-org :type git :host github :repo "misohena/el-easydraw"))
(use-package ob-http)

(require 'eds-org)

(defun my-insert-template (title)
  "Insert a template for an org-roam capture with TITLE."
  (let ((new-title (eds-org/remote-title-boilerplate title)))
    (format "#+title: %s\n" new-title)))

(use-package org-modern
  :after org
  :config
  (with-eval-after-load 'org (global-org-modern-mode)))

(use-package org
  :straight t
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
  (require 'ob-http)
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
  (setopt org-directory (eds-org/get-org-directory)
          org-default-notes-file (concat org-directory "/notes.org")
          eds-org-index-file (concat org-directory "/index.org")
          eds-org-personal-file (concat org-directory "/personal.org")
          eds-org-calendar-file (concat org-directory "/calendar.org")
	  org-refile-targets '((org-agenda-files :maxlevel . 5))
          org-src-fontify-natively t
          org-log-into-drawer t
          org-hide-emphasis-markers t
          org-agenda-include-diary t
          org-timer-default-timer "25"
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
          org-fold-catch-invisible-edits 'smart
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
  (add-hook 'org-clock-in-hook (lambda ()
                                 (if (not org-timer-countdown-timer)
                                     (org-timer-set-timer '(25)))))
  (add-to-list 'org-modules 'org-habit))

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
  (setopt org-roam-mode-sections
          (list #'org-roam-backlinks-section
                #'org-roam-reflinks-section
                #'org-roam-unlinked-references-section))

  (define-key org-roam-mode-map [mouse-1] #'org-roam-visit-thing)
  (setopt org-roam-directory (eds-org/get-org-directory)
        org-roam-completion-everywhere nil
        org-roam-graph-executable "dot"
        org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag))
        org-roam-capture-templates
        `(("d" "default" entry "* ${title}\n%?"
           :target (file+head
                    "%<%Y%m%d%H%M%S>-${slug}.org"
                    ,(concat "#+title: ${title}"
                             "\n#+startup: content"))
           :unnarrowed t)
          ("m" "meeting" entry "* ${title} Notes\n%u\n- [[id:6D43870C-DBA0-4E2D-88D9-3D25BB693FD9][meetings]]\n%?"
           :target
           (file+head
            "%<%Y%m%d%H%M%S>-${slug}.org"
            ,(concat "#+title: ${title}"
                     "\n#+filetags: :meeting:"
                     "\n#+startup: content"
                     "\n* Actions\n"))
           :unnarrowed t)
          ("t" "todo" entry "* TODO %?\n%t\n"
           :target (file+head
                    "%<%Y%m%d%H%M%S>-${slug}.org"
                    ,(concat "#+title: ${title}"
                             "\n#+filetags: :agenda:"
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
          ("i" "idea" entry "* %?\n- [[id:A5284C15-BADD-4A2D-8299-6A8A24339000][Ideas]]\n"
           :target (file+head
                    "%<%Y%m%d%H%M%S>-${slug}.org"
                    ,(concat "#+title: ${title}"
                             "\n#+filetags: :ideas:"
                             "\n#+startup: content"))))
        org-roam-capture-ref-templates
        `(("r" "ref" entry "* ${title}\n${body}\n%?"
           :target
           (file+head
            "%<%Y%m%d%H%M%S>-${slug}.org"
            "%(concat (my-insert-template \"${title}\")
                     \"#+startup: content\n\")")
           :unnarrowed t)
          ("T" "ref" entry "* TODO %?\n%t\n\n${body}\n"
           :target
           (file+head
            "%<%Y%m%d%H%M%S>-${slug}.org"
            ,(concat "#+title: ${title}"
                     "\n#+filetags: :agenda:"
                     "\n#+startup: content"))
           :unnarrowed t)))

  (when (eq system-type 'darwin)
    (setopt org-roam-graph-viewer "/Applications/Firefox.app/Contents/MacOS/firefox"))

  (org-roam-db-autosync-mode)
  (define-key org-roam-mode-map [mouse-1] #'org-roam-preview-visit))

(require 'org-roam-protocol)

(use-package org-roam-ui
  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
    :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start nil))

(provide 'init-org)

;;; init-org.el ends here
