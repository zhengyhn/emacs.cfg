;the cfg for life


;weibo
(add-to-list 'load-path "~/.emacs.d/plugins/weibo")
(require 'weibo)
(global-set-key (kbd "s-w") 'weibo-timeline)

;stardict
(load-file "~/.emacs.d/plugins/sdcv-mode.el")
(require 'sdcv-mode)
(global-set-key (kbd "C-c d") 'sdcv-search)

;douban
;(load-file "~/.emacs.d/plugins/sydio.el")
