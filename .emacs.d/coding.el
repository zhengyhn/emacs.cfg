;;; coding.el --- configuration file for programming
;;
;; Copyright (c) 2013 yuanhang zheng
;;
;; Author: yuanhang zheng <zhengyhn@gmail.com>
;;

;;; code

;;require
(load-file (concat plugins-path "dash.el"))
(require 'dash)

;;comment
(global-set-key (kbd "M-;") 'comment-region)
(global-set-key (kbd "M-'") 'uncomment-region)

;code style
(load-file (concat plugins-path "guess-offset.el"))
(require 'guess-offset)
(load-file (concat plugins-path "smart-tab.el"))

;c
(eval-after-load 'cc-mode
  '(progn
	 (setq c-default-style
		'((c-mode . "k&r")))
	 ;;cscope
	 (load-file (concat plugins-path "xcscope.el"))
	 (require 'xcscope)
	 (global-set-key (kbd "C-'") 'cscope-find-global-definition-no-prompting)
	 (global-set-key (kbd "C-;") 'cscope-pop-mark)
	 (setq cscope-display-cscope-buffer nil)))

;pascal
;(load-file (concat plugins-path "pascal.el"))
;(require 'pascal)
;(autoload 'pascal-mode "pascal" "Pascal Mode." t)
;======================================================

;;php-mode
(load-file (concat plugins-path "php-mode.el"))
(require 'php-mode)

;;css-mode
(eval-after-load 'css-mode
  '(progn
	 (load-file (concat plugins-path "rainbow-mode.el"))
	 (require 'rainbow-mode)

	 (defun css-mode-defaults ()
	   (setq css-indent-offset 2)
	   (rainbow-mode +1))
	 (setq my-css-mode-hook 'css-mode-defaults)
	 (add-hook 'css-mode-hook (lambda ()
								(run-hooks 'my-css-mode-hook)))))

;;haskell
(add-to-list 'load-path (concat plugins-path "haskell-mode"))
(require 'haskell-mode)
(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;smart complie
(load-file "~/.emacs.d/plugins/smart-compile.el")
(global-set-key [f9] 'smart-compile)
(global-set-key [f10] 'gdb)

;auto-complete
(add-to-list 'load-path "~/.emacs.d/plugins/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
			 "~/.emacs.d/plugins/auto-complete/ac-dict")
(ac-config-default)
(setq ac-modes
	  (append ac-modes '(org-mode)
			  '(ielm)))

;;srSpeedbar
(load-file (concat plugins-path "sr-speedbar.el"))
(require 'sr-speedbar)
(global-set-key (kbd "C-c b") 'sr-speedbar-toggle)

;;yaSnippet
(add-to-list 'load-path (concat plugins-path "yasnippet"))
(require 'yasnippet)
(yas-global-mode 1)
;;key
;=====text
;email
;user
;=====c-mode
;once:#ifndef
;main
;inc:#include
;p:printf

(defun insert-empty-line ()
  "insert an empty line after the current line"
  (interactive)
  (move-end-of-line nil)
  (open-line 1)
  (forward-line 1)
  (indent-according-to-mode))
(global-set-key [(shift return)] 'insert-empty-line)

(defun move-line-up ()
  "move up the current line"
  (interactive)
  (transpose-lines 1)
  (forward-line -2))
(global-set-key (kbd "M-P") 'move-line-up)

(defun move-line-down ()
  "move down the current line"
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))
(global-set-key (kbd "M-N") 'move-line-down)

(defun indent-region-or-buffer ()
  "indent a region if selected or the whole buffer"
  (interactive)
  (save-excursion
	(if (region-active-p)
		(progn
		  (indent-region (region-beginning) (region-end))
		  (message "the selectedd region has been indented!"))
	  (progn
		(indent-region (point-min) (point-max))
		(message "the whole buffer has been indented!")))))
(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)

(defun duplicate-current-line-or-region (count)
  "duplicates the current line or region COUNT times"
  (interactive "p")
  (let (beg end (origin (point)))
	(if (and mark-active (> (point) (mark)))
		(exchange-point-and-mark))
	(setq beg (line-beginning-position))
	(if mark-active
		(exchange-point-and-mark))
	(setq end (line-end-position))
	(let ((region (buffer-substring-no-properties beg end)))
	  (-dotimes count
				(lambda (n)
				  (goto-char end)
				  (newline)
				  (insert region)
				  (setq end (point))))
	  (goto-char (+ origin (* (length region) count) count)))))
(global-set-key (kbd "M-d") 'duplicate-current-line-or-region)

(defun rename-file-and-buffer ()
  "renames the current buffer and it's file"
  (interactive)
  (let ((name (buffer-name))
		(filename (buffer-file-name)))
	(if (not (and filename (file-exists-p filename)))
		(message "buffer '%s' is not visiting a file" name)
	  (let ((new-name (read-file-name "new name: " filename)))
		(cond ((get-buffer new-name)
			   (message "a buffer named '%s' exists!" new-name))
			  (t
			   (rename-file name new-name 1)
			   (rename-buffer new-name)
			   (set-visited-file-name new-name)
			   (set-buffer-modified-p nil)))))))
(global-set-key (kbd "C-c r") 'rename-file-and-buffer)

(defun delete-file-and-buffer ()
  "kills the current buffer and deletes its file"
  (interactive)
  (let ((filename (buffer-file-name)))
	(when filename
	  (delete-file filename)
	  (message "file '%s' has been deleted!" filename)))
  (kill-buffer))
(global-set-key (kbd "C-c DEL") 'delete-file-and-buffer)


;;; coding.el ends here
