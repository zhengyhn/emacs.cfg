;;; init.el --- configuration entry file
;;
;; Copyright (c) 2013 yuanhang zheng
;;
;; Author: yuanhang zheng <zhengyhn@gmail.com>
;;

;;; code

(defvar conf-path (file-name-directory load-file-name)
  "root directory of the configuration files")
(defvar plugins-path (expand-file-name "plugins/" conf-path)
  "plugins' directory")

(add-to-list 'load-path conf-path)
(add-to-list 'load-path plugins-path)
(load-file (expand-file-name "ui.el" conf-path))
(load-file (expand-file-name "coding.el" conf-path))
(load-file (expand-file-name "life.el" conf-path))

;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(send-mail-function (quote mailclient-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
