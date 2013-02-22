;=========
;org-mode
;=========
(add-to-list 'load-path (concat plugins-path "org-mode/lisp"))
(add-to-list 'load-path (concat plugins-path "org-mode/contrib/lisp") t)
;;key bindings
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
;; todo list
(setq org-todo-keywords
	  '((sequence "TODO(t)" "DOING(i!)" "HANGUP(h!)" "|"
				  "DONE(d!)" "CANCEL(c!)")))

;;code language
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   (java . t)
   (sh . t)
   (js . t)
   (sql . t)
   (haskell . t)
   (python . t)))

