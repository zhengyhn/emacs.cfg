;; -*- coding: utf-8 -*-
;;
;; Copyright (C) 2013  Haruka Yoshihara <yshr04hrk@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defvar evemacs-notebook-name nil)

(defvar evemacs-evernote-token nil)

(defvar evemacs-info-file (expand-file-name "~/.evemacs.gpg"))

(setq evemacs-el-path
      (file-name-directory (or load-file-name (buffer-file-name))))

(defun evemacs-init ()
  "Init evemacs. If ~/.evemacs.gpg exists, it reads it and token
in it stores to evemacs-token. If doesn't, it runs authorize
app (with sinatra) and waits your input as token."
  (interactive)
  (if (not (null evemacs-evernote-token))
      (message "evemacs is already initialized.")
  (cond ((file-exists-p evemacs-info-file) (evemacs-load-info-file))
        (t
         (when (y-or-n-p "Authorize Evernote? (Using 'browse-url')")
           (browse-url "http://authorize-evemacs.herokuapp.com")
           (let ((evemacs-input-token (read-string "Your token?: ")))
             (cond ((string= "" evemacs-input-token)
                    (message "Please input your token in your browser."))
                   (t
                    (setq evemacs-evernote-token evemacs-input-token)
                    (write-region evemacs-evernote-token nil evemacs-info-file nil nil)))))))))

(defun evemacs ()
  (evemacs-init))

(defun evemacs-load-info-file ()
  (with-temp-buffer
    (insert-file-contents evemacs-info-file)
    (setq evemacs-evernote-token (buffer-string)))
)

(defun evemacs-quoted-string(string)
  (concat "\"" string "\""))

(defun evemacs-command(message notebook)
  (if (null notebook)
      (concat evemacs-el-path "bin/evemacs"
              " -m " (evemacs-quoted-string message)
              " -t " (evemacs-quoted-string evemacs-evernote-token))
      (concat evemacs-el-path "bin/evemacs"
              " -m " (evemacs-quoted-string message)
              " -n " (evemacs-quoted-string notebook)
              " -t " (evemacs-quoted-string evemacs-evernote-token))))

(defun evemacs-send-message(message)
  (interactive "sMessage:")
  (if (null evemacs-evernote-token)
      (evemacs-init))
  (message "Sending the message to your Evernote...")
  (start-process-shell-command "send-to-evernote" "*Messages*"
                               (evemacs-command message evemacs-notebook-name))
  (set-process-sentinel
   (get-process "send-to-evernote")
   '(lambda (process signal)
      (if (string= signal "finished\n")
          (message "Finished.")))))

(provide 'evemacs)
