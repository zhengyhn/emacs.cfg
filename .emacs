(custom-set-variables
 '(inhibit-startup-screen t))

;tabbar-ruler
(add-to-list 'load-path "~/.emacs.d/plugins")
(setq tabbar-ruller-global-tabbar 't)
(setq tabbar-ruler-global-ruler 't)
(require 'tabbar-ruler)

(fset 'yes-or-no-p 'y-or-n-p)
(global-linum-mode t)
(setq x-select-enable-clipboard t)
(setq-default cursor-type 'bar)
(tool-bar-mode -1)

(global-set-key "\C-L" 'forward-char)
(global-set-key "\C-H" 'backward-char)

;theme
(load-theme 'wombat t)



