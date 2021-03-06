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
 '(mode-line-format (quote ("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification "   " mode-line-position (vc-mode vc-mode) "  " mode-line-modes mode-line-misc-info mode-line-end-spaces))))

