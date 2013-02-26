;;jabber

(add-to-list 'load-path (concat plugins-path "emacs-jabber-0.8.91"))
(require 'jabber-autoloads)
(setq jabber-account-list
	  '(("zhengyhn@gmail.com"
		(:password . " mvfcbuvemzqlcead ") 
		(:network-server . "talk.google.com")
		(:connection-type . ssl))))


(global-set-key (kbd "C-c j c") 'jabber-connect-all)
(global-set-key (kbd "C-c j v") '(lambda ()
								   "visit jabber roster"
								   (interactive)
								   (jabber-display-roster)
								   (switch-to-buffer "*-jabber-roster-*")))

