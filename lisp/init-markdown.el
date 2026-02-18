;;; -*- lexical-binding: t -*-
;;; init-markdown.el --- settings related to markdown mode

(require 'eds)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setopt markdown-command "pandoc")
  :bind (("C-c C-e s" . eds/make-svp-contact-link))
  :config
  (add-hook 'markdown-mode-hook 'flyspell-mode))

;; Modified from an elisp tutorial at
;; https://camsaul.com/emacs-lisp/2020/06/09/emacs-lisp-intro-markdown-live-previews-part-1.html
(defun eds/-scroll-percentage ()
  (/ (float (line-number-at-pos (window-start)))
     (float (line-number-at-pos (point-max)))))

(defun eds/-scroll-window-percentage (scroll-percentage)
  (goto-char (point-min))
  (let ((target-line-number (truncate (* (line-number-at-pos (point-max)) scroll-percentage))))
    (forward-line (1- target-line-number)))
  (set-window-start nil (point)))

(defun eds/-insert-markdown-preview-in-buffer (preview-buffer filename &optional scroll-to)
  (shell-command-on-region (point-min) (point-max) "pandoc -f gfm" preview-buffer)
  (switch-to-buffer-other-window preview-buffer)
  (let ((document (libxml-parse-html-region (point-min) (point-max)))
        (url (concat "file://" filename)))
    (erase-buffer)
    (shr-insert-document `(base ((href . ,url)) ,document))
    (setopt buffer-read-only t)
    (if scroll-to
        (eds/-scroll-window-percentage scroll-to))))

(defun eds/-render-markdown-preview-current-buffer ()
  (let ((preview-buffer "*Preview Markdown Output*")
        (scroll-percentage (eds/-scroll-percentage)))
    (message "Rendering Markdown preview of %s" buffer-file-name)
    (save-selected-window
      (eds/-insert-markdown-preview-in-buffer preview-buffer buffer-file-name scroll-percentage))))

(defun eds/-render-markdown-preview (filename)
  (find-file filename)
  (eds/-render-markdown-preview-current-buffer))

(defun eds/preview-markdown (&optional filename)
  "Preview a markdown file FILENAME to html."
  (interactive "fFile: ")
  (if filename
      (progn
        (eds/-render-markdown-preview filename)
        (switch-to-buffer (current-buffer)))
    (eds/-render-markdown-preview-current-buffer)))

(provide 'init-markdown)

;;; markdown.el ends here
