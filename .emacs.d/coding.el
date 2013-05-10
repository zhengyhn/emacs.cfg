;;; coding.el --- configuration file for programming
;;
;; Copyright (c) 2013 yuanhang zheng
;;
;; Author: yuanhang zheng <zhengyhn@gmail.com>
;;

;;; code

;; ido-mode
(require 'ido)
(ido-mode t)

;; comment
(global-set-key (kbd "M-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-'") 'comment-box) 

;; code style
(require 'guess-offset)
;;(load-file (concat plugins-path "smart-tab.el"))

;; c
(eval-after-load 'cc-mode
  '(progn
     (setq c-default-style
	   '((c-mode . "k&r")))

     (require 'xcscope)
     (global-set-key (kbd "C-'")
		     'cscope-find-global-definition-no-prompting)
     (global-set-key (kbd "C-;") 'cscope-pop-mark)
     (setq cscope-display-cscope-buffer nil)))

;;pascal
;;(load-file (concat plugins-path "pascal.el"))
;;(require 'pascal)
;;(autoload 'pascal-mode "pascal" "Pascal Mode." t)


;; php-mode
(autoload 'php-mode "php-mode")
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))

;; css-mode
(eval-after-load 'css-mode
  '(progn
     (require 'rainbow-mode)
     (defun css-mode-defaults ()
       (setq css-indent-offset 4)
       (rainbow-mode +1))
     (setq my-css-mode-hook 'css-mode-defaults)
     (add-hook 'css-mode-hook (lambda ()
				(run-hooks 'my-css-mode-hook)))))

;; java-mode
(eval-after-load 'java-mode
  '(progn
     (defun java-mode-defaults ()
       (setq java-indent-offset 4))
     (setq my-java-mode-hook 'java-mode-defaults)
     (add-hook 'java-mode-hook (lambda ()
				 (run-hooks 'my-java-mode-hook)))))


;; haskell
(add-to-list 'load-path (concat plugins-path "haskell-mode"))
(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
(autoload 'haskell-mode "haskell-mode")
(eval-after-load 'haskell-mode
  '(progn
     (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
     (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
     (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)))

(autoload 'lua-mode "lua-mode")
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))

;; smart complie
(autoload 'smart-compile "~/.emacs.d/plugins/smart-compile.el")
(global-set-key [f9] 'smart-compile)
(global-set-key [f10] 'gdb)

;; auto-complete
(add-to-list 'load-path "~/.emacs.d/plugins/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
	     "~/.emacs.d/plugins/auto-complete/ac-dict")
(ac-config-default)
(setq ac-modes
      (append ac-modes '(org-mode)
	      '(ielm)))

(autoload 'sr-speedbar-toggle "sr-speedbar")
(global-set-key (kbd "C-c b") 'sr-speedbar-toggle)

;; yaSnippet
(add-to-list 'load-path (concat plugins-path "yasnippet"))
(require 'yasnippet)
(yas-global-mode 1)
;; key
;;=====text
;;email
;;user
;;=====c-mode
;;once:#ifndef
;;main
;;inc:#include
;;p:printf

;; all operation function
(autoload 'insert-empty-line "operation")
(global-set-key [(shift return)] 'insert-empty-line)

(autoload 'move-line-up "operation")
(global-set-key (kbd "M-P") 'move-line-up)

(autoload 'move-line-down "operation")
(global-set-key (kbd "M-N") 'move-line-down)

(autoload 'indent-region-or-buffer "operation")
(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)

(autoload 'duplicate-current-line-or-region "operation")
(global-set-key (kbd "M-d") 'duplicate-current-line-or-region)

(autoload 'rename-file-and-buffer "operation")
(global-set-key (kbd "C-c r") 'rename-file-and-buffer)

(autoload 'delete-file-and-buffer "operation")
(global-set-key (kbd "C-c DEL") 'delete-file-and-buffer)

(autoload 'copy-a-line "operation")
(global-set-key (kbd "M-c") 'copy-a-line)

(autoload 'go-to-char "operation")
(global-set-key (kbd "C-c f") 'go-to-char)

(autoload 'kill-all-buffers "operation")
(global-set-key (kbd "C-c k") 'kill-all-buffers)

(autoload 'toggle-transparent "operation")
(global-set-key [(f11)] 'toggle-transparent)

(autoload 'google "operation")
(global-set-key (kbd "C-c g") 'google)


;;; coding.el ends here
