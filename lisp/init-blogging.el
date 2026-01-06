;;; -*- lexical-binding: t -*-
;;; init-blogging.el --- org mode blogging

(use-package ox-hugo
  :after ox)

(defun markdown-convert-buffer-to-org ()
  "Convert the current buffer's content from markdown to orgmode format and save it with the current buffer's file name but with .org extension."
  (interactive)
  (shell-command-on-region (point-min) (point-max)
                           (format "pandoc -f markdown -t org -o %s"
                                   (concat (file-name-sans-extension (buffer-file-name)) ".org"))))

(provide 'init-blogging)

;;; init-blogging.el ends here
