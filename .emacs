;------------------------------------------------
;File:.emacs
;Description:this is my emacs configuration file
;Author:yuanhang zheng
;Upate date:2013-2-8
;Blog:www.itlodge.net
;------------------------------------------------

;=================== start =========================;
(setq cfg-dir "~/.emacs.d/")
(setq plugins-path "~/.emacs.d/plugins/")
(eval-when-compile (require 'cl))

(load-file (concat cfg-dir "coding.el"))
(load-file (concat cfg-dir "display.el"))
(load-file (concat cfg-dir "life.el"))
(load-file (concat cfg-dir "key.el"))
;=================== end ==========================;

