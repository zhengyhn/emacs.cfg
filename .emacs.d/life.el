;the cfg for life

;=======================weibo===========================
(add-to-list 'load-path (concat plugins-path "weibo"))
(require 'weibo)
(global-set-key (kbd "C-c w") 'weibo-timeline)
;=======================================================

;=======================stardict========================
(load-file (concat plugins-path "sdcv-mode.el"))
(require 'sdcv-mode)
(global-set-key (kbd "C-c d") 'sdcv-search)
;=======================================================

;=========================erc===========================
(load-file (concat plugins-path "erc-5.1.2"))
(require 'erc)

;=======================================================

;=======================emacs-w3m===========================================

;(add-to-list 'load-path "/usr/share/emacs/site-lisp/w3m")
;;Enable Cookies
;(setq w3m-use-cookies t)
;;Follow links in W3M
;(setq browse-url-browser-function 'w3m-browse-url browse-url-new-window-flag t)
;(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;(global-set-key "\C-cm" 'browse-url-at-point)
;(global-set-key "\C-cm" 'w3m-browse-url)
;(autoload 'browse-url-interactive-arg "browse-url")
;;Set default download directory
;(let ((d "~/downloads/"))
;(setq w3m-default-save-directory
;	  (or (and (file-directory-p d) d) w3m-default-directory)))
;;W3M doesn't name buffers very intelligently. Let's fix that:
;(add-hook 'w3m-display-hook
;		  (lambda (url)
;			(rename-buffer 
;			 (format "*w3m: %s*" (or w3m-current-title w3m-current-url)) t)))

;=============================================================================

;===============================org-mode======================================

;(setq org-export-odt-preferred-output-format "doc")



;=============================================================================

;douban
;(load-file (concat plugins-path "json.el"))
;(load-file (concat plugins-path "sydio.el"))
