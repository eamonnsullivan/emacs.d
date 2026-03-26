;;; init-utils.el --- Utility functions initialisation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eamonn Sullivan <me@eamonnsullivan.co.uk>

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Maintainer: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Created: 2018-05-03
;; Version: 0.1
;; Package-Requires: ((emacs "24.1"))
;; Keywords: utilities, tools, convenience
;; URL: https://github.com/eamonnsullivan/init-utils

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides initialisation and configuration for utility functions,
;; including support for show-font and workflow enhancements in Emacs.

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

(use-package esup
  :defer t)

(use-package helpful
  :bind
  (([remap describe-function] . helpful-callable)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-key] . helpful-key)
   :map emacs-lisp-mode-map
   ("C-c C-d" . helpful-at-point)))

;; from https://github.com/tychoish/.emacs.d/blob/main/conf/local-functions.el
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setopt deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(use-package writeroom-mode
  :commands (writeroom-mode))

(require 'transient)

(transient-define-prefix cc/isearch-menu ()
  "isearch Menu"
  [["Edit Search String"
    ("e"
     "Edit the search string (recursive)"
     isearch-edit-string
     :transient nil)
    ("w"
     "Pull next word or character word from buffer"
     isearch-yank-word-or-char
     :transient nil)
    ("s"
     "Pull next symbol or character from buffer"
     isearch-yank-symbol-or-char
     :transient nil)
    ("l"
     "Pull rest of line from buffer"
     isearch-yank-line
     :transient nil)
    ("y"
     "Pull string from kill ring"
     isearch-yank-kill
     :transient nil)
    ("t"
     "Pull thing from buffer"
     isearch-forward-thing-at-point
     :transient nil)]

   ["Replace"
    ("q"
     "Start ‘query-replace’"
     isearch-query-replace
     :if-nil buffer-read-only
     :transient nil)
    ("x"
     "Start ‘query-replace-regexp’"
     isearch-query-replace-regexp
     :if-nil buffer-read-only
     :transient nil)]]

  [["Toggle"
    ("X"
     "Toggle regexp searching"
     isearch-toggle-regexp
     :transient nil)
    ("S"
     "Toggle symbol searching"
     isearch-toggle-symbol
     :transient nil)
    ("W"
     "Toggle word searching"
     isearch-toggle-word
     :transient nil)
    ("F"
     "Toggle case fold"
     isearch-toggle-case-fold
     :transient nil)
    ("L"
     "Toggle lax whitespace"
     isearch-toggle-lax-whitespace
     :transient nil)]

   ["Misc"
    ("o"
     "occur"
     isearch-occur
     :transient nil)]])

(define-key isearch-mode-map (kbd "<f2>") 'cc/isearch-menu)

(use-package show-font
  :straight t
  :if (display-graphic-p)
  :commands (show-font-select-preview show-font-list show-font-tabulated)
  :config
  ;; These are the defaults, but I keep them here for easier access.
  (setopt show-font-pangram 'prot)
  (setopt show-font-character-sample
        "
ABCDEFGHIJKLMNOPQRSTUVWXYZ
abcdefghijklmnopqrstuvwxyz
0123456789   !@#$¢%^&*~|
`'\"‘’“”.,;:  ()[]{}—-_+=<>

()[]{}<>«»‹› 6bB8&0ODdoa 1tiIlL|\/
!ij c¢ 5$Ss 7Z2z 9gqp nmMNNMW uvvwWuuw
x×X .,·°;:¡!¿?`'‘’   ÄAÃÀ TODO
")
  (setopt show-font-display-buffer-action-alist '(display-buffer-full-frame)))

(use-package deadgrep)

(use-package tramp
  :defer t
  :init
  (setq tramp-default-method "ssh"))

;; Got the search functions and macro from https://github.com/bbatsov/prelude/blob/master/core/prelude-core.el
(defun search-internet (query-url prompt)
  "Open the search url constructed with the QUERY-URL.
PROMPT sets the `read-string prompt."
  (browse-url
   (concat query-url
           (url-hexify-string
            (if mark-active
                (buffer-substring (region-beginning) (region-end))
              (read-string prompt))))))

(defmacro install-search-engine (search-engine-name search-engine-url search-engine-prompt)
  "Given some information regarding a search engine, install the interactive command to search through them"
  `(defun ,(intern (format "search-%s" search-engine-name)) ()
       ,(format "Search %s with a query or region if any." search-engine-name)
       (interactive)
       (search-internet ,search-engine-url ,search-engine-prompt)))

(install-search-engine "wikipedia"  "http://en.wikipedia.org/wiki/Special:Search/" "Wikipedia: ")
(install-search-engine "kagi"       "http://www.kagi.com/search?q="                "Kagi: ")
(install-search-engine "github"     "https://github.com/search?q="                 "Search GitHub: ")

(global-set-key (kbd "C-c C-/ k") 'search-kagi)
(global-set-key (kbd "C-c C-/ g") 'search-github)
(global-set-key (kbd "C-c C-/ w") 'search-wikipedia)

(provide 'init-utils)
;;; init-utils.el ends here
