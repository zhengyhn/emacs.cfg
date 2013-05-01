;;; operation.el --- configuration file for operation
;;
;; Copyright (c) 2013 yuanhang zheng
;;
;; Author: yuanhang zheng <zhengyhn@gmail.com>
;;

;;; code

(defun insert-empty-line ()
  "insert an empty line after the current line"
  (interactive)
  (move-end-of-line nil)
  (open-line 1)
  (forward-line 1)
  (indent-according-to-mode))

(defun move-line-up ()
  "move up the current line"
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  "move down the current line"
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

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

(defun delete-file-and-buffer ()
  "kills the current buffer and deletes its file"
  (interactive)
  (let ((filename (buffer-file-name)))
	(when filename
	  (delete-file filename)
	  (message "file '%s' has been deleted!" filename)))
  (kill-buffer)) 

(defun copy-a-line ()
  "copy a line"
  (interactive)
  (kill-ring-save (point) (line-end-position))
  (message "copy a line!"))

(defun go-to-char (n char)
  "Move forward to Nth occurence of CHAR"
  (interactive "p\ncGo to char: ")
  (search-forward (string char) nil nil n)
  (while (char-equal (read-char) char)
    (search-forward (string char) nil nil n))
  (setq unread-command-events (list last-input-event)))

(defun kill-all-buffers ()
  "kill all of the buffers"
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun toggle-transparent()
  "transparent frame or not"
  (interactive)
  (if (or
	   (equal (frame-parameter (selected-frame) 'alpha) nil)
	   (equal (frame-parameter (selected-frame) 'alpha) '(100 100)))
	  (set-frame-parameter (selected-frame) 'alpha '(70 50))
	(set-frame-parameter (selected-frame) 'alpha '(100 100))))

(provide 'operation)

(defun google ()
  "google a query or a region"
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
			   (buffer-substring (region-beginning) (region-end))
			 (read-string "google: "))))))

;;; operation.el ends here
