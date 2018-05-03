;;; -*- lexical-binding: t -*-
;;; init-elfeed.el --- managing atom and RSS feeds

(use-package elfeed
  :ensure t
  :config
  (setq-default elfeed-search-filter "@1-week-ago +unread ")
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-url "youtube\\.com"
                                :add '(video youtube)))
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :before "2 weeks ago"
                                :remove 'unread)))

(use-package elfeed-org
  :ensure t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/Dropbox/org/elfeed.org")))

(provide 'init-elfeed)
