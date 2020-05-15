;;; -*- lexical-binding: t -*-
;;; init-proxy.el --- Set up the internet proxy, if necessary.

(defvar *long-proxy*)
(defvar *short-proxy*)
(defvar *proxy-host*)
(defvar *no-proxy*)
(defvar no-proxy-hosts (mapconcat 'identity '("*.core.bbc.co.uk"
                                              "*.gateway.bbc.co.uk"
                                              "*.sandbox.dev.bbc.co.uk"
                                              "*.saucelabs.com"
                                              "*.local"
                                              "169.254.*"
                                              "localhost"
                                              "127.0.0.1"
                                              "0.0.0.0"
                                              "192.168.192.*") "\\|"))
(setq *long-proxy* "http://www-cache.reith.bbc.co.uk:80"
  *short-proxy* "www-cache.reith.bbc.co.uk:80"
  *proxy-host* "www-cache.reith.bbc.co.uk"
  *no-proxy* (concat "\\(" no-proxy-hosts "\\)"))

(defun turn-proxy-on ()
  "Turn the proxy on."
  (interactive)
  (progn
    (setenv "http_proxy"  *long-proxy*)
    (setenv "HTTP_PROXY"  *long-proxy*)
    (setenv "HTTPS_PROXY" *long-proxy*)
    (setenv "ftp_proxy" *long-proxy*)
    (setq url-proxy-services
          `(("no_proxy" . ,*no-proxy*)
            ("http"     . ,*short-proxy*)
            ("https"    . ,*short-proxy*)
            ("ftp"      . ,*short-proxy*)))
    (message "Proxy services enabled.")))

(defun turn-proxy-off ()
  "Turn the proxy off."
  (interactive)
  (progn
    (setq url-proxy-services nil)
    (message "Proxy services disabled.")))

;; (defun proxy-check ()
;;   "Turn proxy on if we can see the proxy server, or off
;; otherwise. Always turn it off on Linux (which never has a
;; proxy)."
;;   (if (or (eq system-type 'gnu/linux)
;;           (string-match "Name or service not known"
;;                         (shell-command-to-string
;;                          (format "ping -c 1 %s" *proxy-host*)))
;;           (string-match "Unknown host"
;;                         (shell-command-to-string
;;                          (format "ping -c 1 %s" *proxy-host*)))
;; 	  (string-match "Temporary failure"
;; 			(shell-command-to-string
;; 			 (format "ping -c 1 %s" *proxy-host*))))
;;       (turn-proxy-off)
;;     (turn-proxy-on)))
;; (proxy-check)

(defun show-current-proxy ()
  "Show whether we have a proxy set."
  (interactive)
  (if url-proxy-services
      (message "Current proxy is \"%s\"" *proxy-host*)
    (message "No proxy")))

(provide 'init-proxy)
