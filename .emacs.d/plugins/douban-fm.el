;;; douban-fm.el --- Listening douban music

;; Copyright (C) 2013 Yuanhang Zheng

;; Author: Yuanhang Zheng <zhengyhn@gmail.com>
;; Version: 0.1
;; Created: Wed May  1 11:42:08 2013
;; Use two plugins:
;; FMD: https://github.com/hzqtc/fmd
;; FMC: https://github.com/hzqtc/fmc

(defun fmc-play ()
  "if not start, start fmd and play, else, pause"
  (interactive)
  (if (equal (shell-command-to-string "ps -e | grep fmd") "")
      (progn
	(shell-command "fmd")
	(sleep-for 1)
	(shell-command "fmc play"))
    (progn
      (shell-command "fmc toggle"))))

(defun fmc-next ()
  "play next song"
  (interactive)
  (shell-command "fmc skip"))
(global-set-key (kbd "C-c m n") 'fmc-next)

(defun fmc-favor ()
  "mark the current song as favor"
  (interactive)
  (shell-command "fmc rate"))
(global-set-key (kbd "C-c m f") 'fmc-favor)

(defun fmc-trash ()
  "don't play the current song again"
  (interactive)
  (shell-command "fmc ban"))

(provide 'douban-fm)
