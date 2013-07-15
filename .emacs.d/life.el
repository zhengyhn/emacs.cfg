;;; life.el --- configuration file for beautiful life
;;
;; Copyright (c) 2013 yuanhang zheng
;;
;; Author: yuanhang zheng <zhengyhn@gmail.com>
;;

;;; code

;;for goagent
(setq url-proxy-services '(("http" . "proxy-shz.intel.com:911")
			   ("socks" . "proxy-shz.intel.com:1080")
			   ("https" . "proxy-shz.intel.com:911")))

;; org-mode
(add-to-list 'load-path (concat plugins-path "org-mode/lisp"))
(add-to-list 'load-path (concat plugins-path "org-mode/contrib/lisp") t)
(eval-after-load "org-mode"
  '(progn
     (global-set-key (kbd "S-TAB") 'pcomplete)
     (setq org-todo-keywords
	   '((sequence "TODO(t)" "DOING(i!)" "DONE(d!)")))
     (org-babel-do-load-languages
      'org-babel-load-languages
      '((C . t)
	(C++ . t)
	(java . t)
	(sh . t)
	(js . t)
	(sql . t)
	(haskell . t)
	(python . t)))
     (setq org-export-default-language "zh-CN")
     (add-hook 'org-mode-hook
	       (lambda ()
		 (setq truncate-liness nil)))))

;; weibo
(add-to-list 'load-path (concat plugins-path "weibo"))
(autoload 'weibo-timeline "weibo")
(global-set-key (kbd "C-c w") 'weibo-timeline)

;; jabber-mode
(add-to-list 'load-path (concat plugins-path "emacs-jabber-0.8.91"))
(require 'jabber-autoloads)
(global-set-key (kbd "C-c j c") 'jabber-connect-all)
(eval-after-load "jabber-autoloads"
  '(progn
     (require 'netrc)
     (setq cred (netrc-machine (netrc-parse "~/.authinfo") "jabber" t))
     (setq jabber-account-list
	   `((, (netrc-get cred "login")
		(:password . , (netrc-get cred "password"))
		(:network-server . "talk.google.com")
		(:connection-type . ssl)
		(:port . 5223))))
     (global-set-key
      (kbd "C-c j v") '(lambda ()
			 "visit jabber roster"
			 (interactive)
			 (jabber-display-roster)
			 (switch-to-buffer "*-jabber-roster-*")))))

;; stardict
(autoload 'sdcv-search "sdcv-mode")
(global-set-key (kbd "C-c d") 'sdcv-search)

;; emms
(add-to-list 'load-path (concat plugins-path "emms-3.0"))
(global-set-key (kbd "C-c e g") 'emms-play-directory)    ;go
(autoload 'emms-play-directory "emms-setup")
(autoload 'emms-play-directory "emms-player-mplayer")
(eval-after-load "emms-setup"
  '(progn
     (setq exec-path (append exec-path '("/usr/bin")))
     (emms-standard)
     (emms-default-players)
     (setq emms-player-list '(emms-player-mplayer)
	   emms-player-mplayer-command-name "mplayer"
	   emms-player-mplayer-parameters '("-slave"))
     (setq emms-repeat-playlist t
	   emms-source-file-default-directory "~/movie/music"
	   emms-lyrics-dir "~/movie/music"
	   emms-lyrics-coding-system nil
	   emms-playlist-buffer-name "*EMMS*")
     (global-set-key (kbd "C-c e v") 'emms-playlist-mode-go)  ;visit
     (global-set-key (kbd "C-c e s") 'emms-stop)
     (global-set-key (kbd "C-c e n") 'emms-next)
     (global-set-key (kbd "C-c e p") 'emms-previous)
     (global-set-key (kbd "C-c e r") 'emms-shuffle)           ;random
     (global-set-key (kbd "C-c e SPC") 'emms-pause)
     (global-set-key (kbd "C-c e d") 'emms-play-dired)
     (global-set-key (kbd "C-c e x") 'emms-start)))

;; youdao dictionary
(add-to-list 'load-path (concat plugins-path "youdao-dict"))
(autoload 'youdao-dict "youdao-dict")
(global-set-key (kbd "C-c y") 'youdao-dict)

;; douban fm
(autoload 'fmc-play "douban-fm")
(global-set-key (kbd "C-c m p") 'fmc-play)

;; evernote
(add-to-list 'load-path (concat plugins-path "evemacs"))
(autoload 'evemacs-send-message "evemacs")
(global-set-key (kbd "C-c n s") 'evemacs-send-message)

;; muse
;;(require 'muse-mode)
;;(autoload 'muse-html "muse-mode")

;; mu4e
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(autoload 'mu4e "mu4e")
(global-set-key (kbd "C-x m") 'mu4e)

(eval-after-load 'mu4e
  '(progn
     (setq mu4e-maildir "~/Maildir")
     (setq mu4e-drafts-folder "/[Gmail].Drafts")
     (setq mu4e-sent-folder   "/[Gmail].Sent Mail")
     (setq mu4e-trash-folder  "/[Gmail].Trash")
     (setq mu4e-sent-messages-behavior 'delete)

     (setq mu4e-maildir-shortcuts
	   '( ("/INBOX"               . ?i)
	      ("/[Gmail].Sent Mail"   . ?s)
	      ("/[Gmail].Trash"       . ?t)
	      ("/[Gmail].All Mail"    . ?a)))
     (setq mu4e-get-mail-command "offlineimap")

     (setq
      user-mail-address "zhengyhn@gmail.com"
      user-full-name  "yuanhang zheng"
      message-signature
      (concat
       "郑远航(yuanhang zheng)\n"
       "Email: zhengyhn@gmail.com\n"
       "Blog: www.zhengyuanhang.com\n"))

     (setq mu4e-view-prefer-html t)
     (setq mu4e-attachment-dir "~/downloads")))  ;; A to view attachment
     (global-set-key (kbd "C-c C-a s") 'mu4e-view-save-attachment)

(autoload 'smtpmail "smtpmail")
(eval-after-load 'smtpmail
  '(progn
     (setq message-send-mail-function 'smtpmail-send-it
	   smtpmail-stream-type 'starttls
	   smtpmail-default-smtp-server "smtp.gmail.com"
	   smtpmail-smtp-server "smtp.gmail.com"
	   smtpmail-smtp-service 587)
     (setq message-kill-buffer-on-exit t)
     ;; for attachment
     ;; M-x dired, mark the file(s), and C-c RET C-a
     (require 'gnus-dired)
     (defun gnus-dired-mail-buffers ()
       "Return a list of active message buffers"
       (let (buffers)
	 (save-current-buffer
	   (dolist (buffer (buffer-list t))
	     (set-buffer buffer)
	     (when (and (derived-mode-p 'message-mode)
			(null message-sent-message-via))
	       (push (buffer-name buffer) buffers))))
	 (nreverse buffers)))
     (setq gnus-dired-mail-mode 'mu4e-user-agent)
     (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)))

;; octopress
(autoload 'octo-preview "em-octopress")
(global-set-key (kbd "C-c o p") 'octo-preview)
(global-set-key (kbd "C-c o n") 'octo-new-post)
(global-set-key (kbd "C-c o d") 'octo-deploy)

;;; life.el ends here

