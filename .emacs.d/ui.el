;;; ui.el --- configuration file for beautiful user interface
;;
;; Copyright (c) 2013 yuanhang zheng
;;
;; Author: yuanhang zheng <zhengyhn@gmail.com>
;;

;;; code

(custom-set-variables
 '(mode-line-format (quote ("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification "   " mode-line-position (vc-mode vc-mode) "  " mode-line-modes mode-line-misc-info mode-line-end-spaces))))

(setq frame-title-format "%F - %f")

(setq inhibit-startup-screen t)    ;; don't show welcome buffer
(tool-bar-mode -1)
(menu-bar-mode -1)                 ;; disable toolbar,menubar and scrollbar
(scroll-bar-mode -1)

(display-time)                     ;; display clock
;;(global-linum-mode t)              ;; show line and column number
(setq column-number-mode t)

(fset 'yes-or-no-p 'y-or-n-p)      ;; y or n instead of yes or no
(setq x-select-enable-clipboard t) ;; share clipboard

(global-hl-line-mode t)            ;; highlight the current line
(blink-cursor-mode -1)
(global-font-lock-mode t)

;; theme
(load-theme 'dichromacy t)         ;; select the theme
;;(load-theme 'adwaita t)
;;(load-theme 'deeper-blue)
;;(load-theme 'light-blue)
;;(load-theme 'manoj-dark)
;;(load-theme 'misterioso)
;;(load-theme 'tango-dark)
;;(load-theme 'tango)
;;(load-theme 'tsdh-dark)
;;(load-theme 'tsdh-light)
;;(load-theme 'wheatgrass)
;;(load-theme 'whiteboard)
;;(load-theme 'wombat)
;;(load-file (concat plugins-path "color-theme.el"))
;;(require 'color-theme)
;;(load-file (concat conf-path "my-theme.el"))
;;(my-theme)

;;tabbar-ruler
(add-to-list 'load-path "~/.emacs.d/plugins/tabbar")
(require 'tabbar)
;;(setq tabbar-ruler-global-tabbar 't)
;;(setq tabbar-ruler-global-ruler 't)
;;(require 'tabbar-ruler)
(tabbar-mode t)
(global-set-key [C-tab] 'tabbar-forward)             ;;switch tabbar
(global-set-key [C-S-iso-lefttab] 'tabbar-backward)

;;encoding
(require 'unicad)

;;all *~ files in one place
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(global-reveal-mode t)
;;(electric-pair-mode t)
(show-paren-mode +1)
(setq show-paren-style 'parenthesis)
(icomplete-mode +1)

;; font
(set-default-font "-unknown-文泉驿等宽微米黑-normal-normal-normal-*-*-*-*-*-*-0-iso10646-1")

;; user info
(setq user-full-name "yuanhang zheng"
      user-mail-address "zhengyhn@gmail.com")

;; key bindings
(global-set-key (kbd "C-SPC") nil)          ;;disable for fcitx input method

(global-set-key (kbd "C-c t") 'eshell)       ;;start eshell,'t' for terminal
(eval-after-load 'eshell
  '(progn
     (require 'exec-path-from-shell)
     (exec-path-from-shell-initialize)
     (require 'ansi-color)
     (defun eshell-colorize-buffer ()
       (ansi-color-apply-on-region (point-min) (point-max)))
     (add-hook 'eshell-output-filter-functions 'eshell-colorize-buffer)
     (defun eshell-rename-buffer ()
       "Rename eshell buffer"
       (if (eq major-mode 'eshell-mode)
	   (rename-buffer
	    (concat
	     "esh-" (car (last (butlast (split-string
					 default-directory "/"))))) t)))
     (add-hook 'eshell-output-filter-functions 'eshell-rename-buffer)))

;;(global-set-key (kbd "C-c t") 'shell)       ;;start shell,'t' for terminal
(eval-after-load 'shell
  '(progn
     (require 'exec-path-from-shell)
     (exec-path-from-shell-initialize)
     ;; With this, we can open more than one buffer for shell
     (defun shell-mode-auto-rename-buffer (text)
       "Auto rename shell buffer"
       (if (eq major-mode 'shell-mode)
	   (rename-buffer
	    (concat
	     "shell-" (car (last (butlast
				  (split-string default-directory "/"))))) t)))
     (add-hook 'comint-output-filter-functions
	       'shell-mode-auto-rename-buffer)))


(global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)   ;;ctrl+mouse 
(global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease)   ;;font size

(global-set-key (kbd "C-c i") 'image-dired)   ;;for image view

(global-set-key "\C-L" 'forward-char)     ;;origin is clear to center
(global-set-key "\C-H" 'backward-char)    ;;origin is help
(global-set-key (kbd "RET") 'newline-and-indent)   ;;for indent
(global-set-key (kbd "<f5>") 'revert-buffer)   ;; reload the file

;;; ui.el ends here
