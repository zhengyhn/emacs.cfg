;;jabber

(add-to-list 'load-path (concat plugins-path "emacs-jabber-0.8.91"))
(require 'jabber-autoloads)
(setq jabber-account-list
	  '(("zhengyhn@gmail.com"
		(:network-server . "talk.google.com")
		(:connection-type . ssl))))
(global-set-key (kbd "C-c j c") 'jabber-connect-all)
(global-set-key (kbd "C-c j v") 'jabber-display-roster)   ;visit

