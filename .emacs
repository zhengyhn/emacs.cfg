;------------------------------------------------
;File:.emacs
;Description:this is my emacs configuration file
;Author:monkey
;Upate date:2012-11-25
;Blog:www.itlodge.net
;------------------------------------------------

;=================== start =========================;

(defvar cfg_dir "~/.emacs.d/")

(load-file "~/.emacs.d/coding.el")

(load-file "~/.emacs.d/display.el")

(load-file "~/.emacs.d/key.el")

(load-file "~/.emacs.d/life.el")


;=================== end ==========================;

(custom-set-variables
 '(ecb-options-version "2.40"))
(custom-set-faces)

