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

(add-to-list 'load-path default-directory)

(when (null (require 'ert nil t))
  (shell-command "wget https://raw.github.com/ohler/ert/c619b56c5bc6a866e33787489545b87d79973205/lisp/emacs-lisp/ert.el")
  (when (null (require 'ert nil t))
  (message "Fail to load 'ert.el'. task test:elisp is skiped.")))

(require 'evemacs)

(setq default-directory (expand-file-name default-directory))

(setq evernote-token "evernote-token")
(setq evemacs-evernote-token evernote-token)

(ert-deftest evemacs:evemacs-command-with-notebook ()
  (let ((message "This is message!")
        (notebook "evemacs-notebook"))
    (setq expected-command
          (format "%sbin/evemacs -m \"%s\" -n \"%s\" -t \"%s\""
                  default-directory message notebook evernote-token))
    (should (equal expected-command (evemacs-command message notebook)))))

(ert-deftest evemacs:evemacs-command-without-notebook ()
  (let ((message "This is message!")
        (notebook nil))
    (setq expected-command
          (format "%sbin/evemacs -m \"%s\" -t \"%s\""
                  default-directory message evernote-token))
    (should (equal expected-command (evemacs-command message notebook)))))

(ert-deftest evemacs:evemacs-quoted-string ()
  (let ((message   "This is the message"))
    (should (equal "\"This is the message\"" (evemacs-quoted-string message)))))

(ert-deftest evemacs:evemacs-load-info-file ()
  (setq evemacs-info-file (file-relative-name "test/fixtures/info-file.txt"))
  (evemacs-load-info-file)
  (should (equal "This is the info file.\n" evemacs-evernote-token)))

(ert-run-tests-batch-and-exit)
