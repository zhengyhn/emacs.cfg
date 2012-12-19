;the configuration for key binding

;customize key
(global-set-key "\C-L" 'forward-char)
(global-set-key "\C-H" 'backward-char)

(global-set-key (kbd "C-SPC") nil)          ;disable for fcitx input method

(global-set-key (kbd "RET") 'newline-and-indent)   ;for indent

(global-set-key (kbd "C-c s") 'shell)       ;start shell

(global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)   ;ctrl+mouse change
(global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease)   ;font size
