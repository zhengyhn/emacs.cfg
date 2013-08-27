;;; em-octopress A tool for posting to octopress

;; Copyright (C) 2013 Yuanhang Zheng

;; Author: Yuanhang Zheng <zhengyhn@gmail.com>
;; Version: 0.1
;; Created: Sun Jun 30 21:14:14 2013
;; Updated: Mon Jul  1 20:53:55 2013
;; URL:

(defconst octo-dir (expand-file-name "~/git/octopress"))
(defconst octo-rake (expand-file-name "~/.gem/ruby/1.9.1/bin/bundle exec rake"))
(defconst octo-default-preview-url "http://localhost:4000")
(setq password nil)

(defun octo-do (what)
  "Execute the octopress command"
  (shell-command-to-string
   (format "bash -l -c 'eval \"$(rbenv init -)\" && rbenv local 1.9.3-p194 &&
cd %s && %s %s'" octo-dir octo-rake what)))

(defun octo-preview ()
  "Preview the blog in the default browser"
  (interactive)
  (browse-url octo-default-preview-url)
  "Ctrl-g to cancel"
  (octo-do "preview"))

(defun octo-new-post ()
  "Create an article."
  (interactive)
  
  (let* ((title (read-string "Please input the title: "))
         (command (format "new_post[\"%s\"]" title))
         (result (octo-do command))
         (regexp-str "Creating new post: ")
         (filename (concat octo-dir "/"
                           (replace-regexp-in-string
                            regexp-str ""
                            (car (cdr (split-string result "\n")))))))
    "And Open the file"
      (find-file filename)))

(defun octo-generate ()
  "Generate the blog"
  (interactive)
  (message "Generating...")
  (octo-do "generate")
  (message "Generated"))

(defun octo-deploy ()
  "Deploy the blog"
  (interactive)
  (octo-generate)
  (shell-command-to-string
   (format "cd %s && mkdir -p public/_posts/ && mkdir -p public/me" octo-dir))
  (message "Deploying...")
  (octo-do "deploy")
  (sleep-for 3)
  (if (not password)
      (setq password (read-passwd "Please input the password: ")))
  (message "Committing the source")
  (shell-command (format (concat "bash -l -c "
                                 "'cd %s && "
                                 "git add -A && "
                                 "git commit -a -m \"update\" && "
                                 "echo \"%s\" | git push origin source'")
                         octo-dir password))
  (message "Deployed"))

(provide 'em-octopress)

;; em-octopress.el ends here

