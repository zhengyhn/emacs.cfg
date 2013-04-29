;;; ui.el --- configuration file for beautiful user interface
;;
;; Copyright (c) 2013 yuanhang zheng
;;
;; Author: yuanhang zheng <zhengyhn@gmail.com>
;;

;;; code

(setq frame-title-format "%F - %f")

(setq inhibit-startup-screen t)    ;don't show welcome buffer
(tool-bar-mode -1)
(menu-bar-mode -1)                 ;disable toolbar,menubar and scrollbar
(scroll-bar-mode -1)

(fset 'yes-or-no-p 'y-or-n-p)      ;y or n instead of yes or no
(display-time)                     ;display clock
(global-linum-mode t)              ;show line and column number
(setq column-number-mode t)

(setq x-select-enable-clipboard t) ;share clipboard

(global-hl-line-mode t)            ;highlight the current line
(blink-cursor-mode -1)
(global-font-lock-mode t)

;;theme
(load-theme 'dichromacy t)         ;select the theme
;(load-theme 'adwaita t)
;(load-theme 'deeper-blue)
;(load-theme 'light-blue)
;(load-theme 'manoj-dark)
;(load-theme 'misterioso)
;(load-theme 'tango-dark)
;(load-theme 'tango)
;(load-theme 'tsdh-dark)
;(load-theme 'tsdh-light)
;(load-theme 'wheatgrass)
;(load-theme 'whiteboard)
;(load-theme 'wombat)
;(load-file (concat plugins-path "color-theme.el"))
;(require 'color-theme)
;(load-file (concat conf-path "my-theme.el"))
;(my-theme)

;;tabbar-ruler
(add-to-list 'load-path "~/.emacs.d/plugins/tabbar")
(require 'tabbar)
;(setq tabbar-ruler-global-tabbar 't)
;(setq tabbar-ruler-global-ruler 't)
;(require 'tabbar-ruler)
(tabbar-mode t)
(global-set-key [C-tab] 'tabbar-forward)             ;switch tabbar
(global-set-key [C-S-iso-lefttab] 'tabbar-backward)

;;encoding
;(load-file "~/.emacs.d/plugins/unicad.el")
(require 'unicad)

;;all *~ files in one place
(setq backup-directory-alist
	  `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
	  `((".*" ,temporary-file-directory t)))

(global-reveal-mode t)
;(electric-pair-mode t)
(show-paren-mode +1)
(setq show-paren-style 'parenthesis)
(icomplete-mode +1)

;;font
(set-default-font "-unknown-文泉驿等宽微米黑-normal-normal-normal-*-*-*-*-*-*-0-iso10646-1")

;;transparent
(defun toggle-transparent()
  "transparent frame or not"
  (interactive)
  (if (or
	   (equal (frame-parameter (selected-frame) 'alpha) nil)
	   (equal (frame-parameter (selected-frame) 'alpha) '(100 100)))
	  (set-frame-parameter (selected-frame) 'alpha '(70 50))
	(set-frame-parameter (selected-frame) 'alpha '(100 100))))
(global-set-key [(f11)] 'toggle-transparent)

;;user info
(setq user-full-name "yuanhang zheng"
	  user-mail-address "zhengyhn@gmail.com")

;;key bindings
(global-set-key (kbd "C-SPC") nil)          ;disable for fcitx input method

(global-set-key (kbd "C-c t") 'eshell)       ;start shell,'t' for terminal

(global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)   ;ctrl+mouse change
(global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease)   ;font size

(global-set-key (kbd "C-c i") 'image-dired)   ;for image view

(global-set-key "\C-L" 'forward-char)     ;origin is clear to center
(global-set-key "\C-H" 'backward-char)    ;origin is help
(global-set-key (kbd "RET") 'newline-and-indent)   ;for indent

(defun copy-a-line ()
  "copy a line"
  (interactive)
  (kill-ring-save (point) (line-end-position))
  (message "copy a line!"))
(global-set-key (kbd "M-c") 'copy-a-line)            ;orgin is captilize

(defun go-to-char (n char)
  "Move forward to Nth occurence of CHAR"
  (interactive "p\ncGo to char: ")
  (search-forward (string char) nil nil n)
  (while (char-equal (read-char) char)
    (search-forward (string char) nil nil n))
  (setq unread-command-events (list last-input-event)))
(global-set-key (kbd "C-c f") 'go-to-char)

(defun kill-all-buffers ()
  "kill all of the buffers"
  (interactive)
  (mapc 'kill-buffer (buffer-list)))
(global-set-key (kbd "C-c k") 'kill-all-buffers)

;;; ui.el ends here
