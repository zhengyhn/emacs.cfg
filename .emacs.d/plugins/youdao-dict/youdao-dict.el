;;; youdao-dict --- Query English words with youdao dictionary

;; Copyright (C) 2013 Yuanhang Zheng

;; Author: Yuanhang Zheng <zhengyhn@gmail.com>
;; Version: 0.1
;; Created: Sun Mar 31 12:36:27 2013
;; Updated: Sun Jun 30 11:52:12 2013
;; URL: https://github.com/itlodge/youdao-dict

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
    (if (string-match "[,<.>/?;:\[{}`~!@#$%^&*()=+]" word)
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
      (error "Opps!Word not found!"))
    
    (setq phonetic-symbol (car (last (car (xml-get-children
					   youdao-dict 'phonetic-symbol)))))
    ;; get translations
    (setq translations (xml-get-children
			(car (xml-get-children
			      youdao-dict 'custom-translation)) 'translation))
    (setq trans-content '())
    (loop for trans in translations
	  do (push (car (last (car (xml-get-children
				    trans 'content))))
		   trans-content))
    ;; get word forms
    (setq wordforms (xml-get-children
		     (car (xml-get-children
			   youdao-dict 'word-forms)) 'word-form))
    (setq form-content '())
    (loop for wf in wordforms
	  do (push (concat (car (last (car (xml-get-children wf 'name))))
			   ": "
			   (car (last (car (xml-get-children wf 'value)))))
		   form-content))

    ;; get example sentences
    (setq sentences (xml-get-children
		     (car (xml-get-children
			   youdao-dict 'example-sentences)) 'sentence-pair))
    (setq sen-content '())
    (loop for sen in sentences
	  do (progn
	       (let* ((sen
		       (car (last (car (xml-get-children sen 'sentence)))))
		      (sen (replace-regexp-in-string "<b>" "" sen))
		      (sen (replace-regexp-in-string "</b>" "" sen)))
		 (push sen sen-content))
	       (push
		(car (last (car (xml-get-children sen 'sentence-translation))))
		sen-content)))
    
    (setq result
	  (append
	   (list
	    (concat "Query: " (if original-query original-query))
	    (concat "Return: " (if return-phrase return-phrase))
	    (if phonetic-symbol
		(concat "Phonetic: /" phonetic-symbol "/")))
	   (if trans-content (reverse trans-content))
	   (if form-content (reverse form-content))
	   (if sen-content (reverse sen-content))))))

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
    (popup-menu* (youdao-dict-parse-xml xml-result)
		 :margin t)))

(provide 'youdao-dict)

;;; youdao.el ends here
