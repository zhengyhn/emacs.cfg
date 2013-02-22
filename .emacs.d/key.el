;the configuration for key binding

(global-set-key (kbd "C-SPC") nil)          ;disable for fcitx input method

(global-set-key (kbd "C-c t") 'shell)       ;start shell,'t' for terminal

(global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)   ;ctrl+mouse change
(global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease)   ;font size

(global-set-key (kbd "C-c i") 'image-dired)   ;for image view

(global-set-key "\C-L" 'forward-char)     ;origin is clear to center
(global-set-key "\C-H" 'backward-char)    ;origin is help
;(global-set-key "\C-M" 'backward-delete-char)   ;origin is RET
(global-set-key (kbd "RET") 'newline-and-indent)   ;for indent

(global-set-key (kbd "M-c")                   ;orgin is captilize
				(lambda ()
				  "copy a line"
				  (interactive)
				  (kill-line)
				  (defvar r nil)
				  (point-to-register r)   ;copy current point to register
				  (yank)
				  (jump-to-register r)))  ;jump to the point
