;the cfg for life

;weibo
(add-to-list 'load-path (concat plugins-path "weibo"))
(require 'weibo)
(global-set-key (kbd "C-c w") 'weibo-timeline)

;stardict
(load-file (concat plugins-path "sdcv-mode.el"))
(require 'sdcv-mode)
(global-set-key (kbd "C-c d") 'sdcv-search)



;douban
;(load-file (concat plugins-path "json.el"))
;(load-file (concat plugins-path "sydio.el"))
