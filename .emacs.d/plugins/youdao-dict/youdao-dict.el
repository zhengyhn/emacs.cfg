;;; youdao-dict --- Query English words with youdao dictionary

;; Copyright (C) 2013 Yuanhang Zheng

;; Author: Yuanhang Zheng <zhengyhn@gmail.com>
;; Version: 0.1
;; Created: Sun Mar 31 12:36:27 2013
;; URL: https://github.com/itlodge/youdao-dict

(load-file (concat (file-name-directory load-file-name) "pos-tip-0.4.5.el"))
(require 'popup)

(defconst SEARCH-URL "'http://dict.youdao.com/search?le=eng&keyfrom=dict.index&xmlDetail=true&doctype=xml&q=%s'")
(defconst SEARCH-PROMPT "Youdao for (default %s): ")

(defun read-word ()
  "Read word from active region or keyboard"
  (let (word)
    (setq word
	  (if mark-active
	      (buffer-substring
	       (region-beginning) (region-end))
	    (read-string
	     (format SEARCH-PROMPT (current-word nil t))
	     nil nil (current-word nil t))))
    ;; drop the whitespace at start or end
    (setq word (when (string-match "^[ \t]*" word)
		 (replace-match "" nil nil word)))
    (setq word (when (string-match "[ \t]*$" word)
		 (replace-match "" nil nil word)))
    ;; no punctuation mark
    (if (string-match "[,<.>/?;:\[{}`~!@#$%^&*()-=+]" word)
	(error "Wrong word!"))
    (setq word word)))

(defun youdao-dict-parse-xml (xml-result)
  "Parse the xml crawled and return the useful content"
  (let (buffer youdao-dict original-query return-phrase phonetic-symbol
	       translations trans-content word-forms form-content
	       sentences sen-content result)
    (setq buffer (with-temp-buffer
		   (insert xml-result)
		   (xml-parse-region (point-min) (point-max))))
    ;; the root
    (setq youdao-dict (car buffer))
    (setq original-query (car (last (car (xml-get-children
					  youdao-dict 'original-query)))))
    (setq return-phrase (car (last (car (xml-get-children
					 youdao-dict 'return-phrase)))))
    (when (equal return-phrase nil)
      (pos-tip-show "Opps!Word not found!" '("red" . "gray"))
      (error "Opps!Word not found!"))
    
    (setq phonetic-symbol (car (last (car (xml-get-children
					   youdao-dict 'phonetic-symbol)))))
    ;; get translations
    (setq translations (xml-get-children
			(car (xml-get-children
			      youdao-dict 'custom-translation)) 'translation))
    (setq trans-content nil)
    (loop for trans in translations
	  do (setq trans-content
		   (concat trans-content
			   (car (last (car (xml-get-children
					    trans 'content)))) "\n")))
    ;; get word forms
    (setq wordforms (xml-get-children
		     (car (xml-get-children
			   youdao-dict 'word-forms)) 'word-form))
    (setq form-content nil)
    (loop for wf in wordforms
	  do (setq form-content
		   (concat form-content
			   (car (last (car (xml-get-children wf 'name))))
			   ": "
			   (car (last (car (xml-get-children wf 'value))))
			   "\n")))
    ;; get example sentences
    (setq sentences (xml-get-children
		     (car (xml-get-children
			   youdao-dict 'example-sentences)) 'sentence-pair))
    (setq sen-content nil)
    (loop for sen in sentences
	  do (setq sen-content
		   (concat sen-content
			   (car (last (car
				       (xml-get-children sen 'sentence))))
			   "\n"
			   (car (last (car
				       (xml-get-children
					sen 'sentence-translation))))
			   "\n")))
    ;; drop html tag <b></b>
    (setq sen-content (replace-regexp-in-string "<b>" "" sen-content))
    (setq sen-content (replace-regexp-in-string "</b>" "" sen-content))
    
    (setq result (concat "Query: " (if original-query original-query)  "\n"
			 "Return: " (if return-phrase return-phrase) "\n"
			 (if phonetic-symbol
			     (concat "phonetic: /" phonetic-symbol "/\n"))
			 (if trans-content trans-content)
			 (if form-content form-content)
			 (if sen-content sen-content)))))

(defun youdao-dict ()
  "Query a word from youdao online dictionaries and show it."
  (interactive)
  (let (word-to-query xml-result)
    (setq word-to-query (read-word))
    (setq xml-result (shell-command-to-string
		      (concat "curl "
			      (format SEARCH-URL
				      (url-hexify-string word-to-query))
			      " 2> /dev/null")))
    (popup-tip (youdao-dict-parse-xml xml-result))))

(provide 'youdao-dict)

;;; youdao.el ends here
