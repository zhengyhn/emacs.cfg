;------------------------------------------------
;File:.emacs
;Description:this is my emacs configuration file
;Author:monkey
;Upate date:2012-11-25
;Blog:www.itlodge.net
;------------------------------------------------

;=================== start =========================;

(setq cfg-dir "~/.emacs.d/")
(setq plugins-path "~/.emacs.d/plugins/")

(load-file (concat cfg-dir "coding.el"))

(load-file (concat cfg-dir "display.el"))

(load-file (concat cfg-dir "key.el"))

(load-file (concat cfg-dir "life.el"))


;=================== end ==========================;

(custom-set-variables
 '(ecb-options-version "2.40"))
(custom-set-faces)

