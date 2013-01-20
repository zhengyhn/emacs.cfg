;------------------------------------------------
;File:.emacs
;Description:this is my emacs configuration file
;Author:monkey
;Upate date:2013-1-13
;Blog:www.itlodge.net
;------------------------------------------------

;=================== start =========================;

(setq cfg-dir "~/.emacs.d/")
(setq plugins-path "~/.emacs.d/plugins/")
(eval-when-compile (require 'cl))

(load-file (concat cfg-dir "coding.el"))

(load-file (concat cfg-dir "display.el"))

(load-file (concat cfg-dir "key.el"))

(load-file (concat cfg-dir "life.el"))


;=================== end ==========================;


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/.emacs.d/org/weather.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
