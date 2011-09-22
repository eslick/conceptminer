;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: eventminer -*-

(in-package :conceptminer)

;; ===================================
;; YAHOO BASED URL LOOKUP
;; ===================================

(defclass yahoo-searcher (document-searcher)
  ((type :accessor searcher-content-type :initarg :content-type :initform :web))) 

(defmethod search-for-phrase ((s yahoo-searcher) phrase-as-string &key &allow-other-keys)
  (do-search s (concatenate "\"" phrase-as-string "\"")))

(defmethod do-search ((s yahoo-searcher) query &key &allow-other-keys)
  (search-yahoo query (searcher-max-refs s) :type (searcher-content-type s)))

(defun-exported search-yahoo (query pages &key (type :web) &aux (retries 1))
  (loop for count from 1 upto (ceiling pages 50)
	nconcing
	(handler-case
	 (mapcar #'cl-yahoo:url (cl-yahoo:ysearch type
						  :query query
						  :adult_ok nil 
						  :similar_ok nil
						  :start (+ (* (1- count) 50) 1)
						  :results 50))
	 (error () 
		(if (> (incf retries) 60)
		    (error "Unable to access Yahoo query server for 2 minutes.")
		  (progn
		    (write-log miner "Failed to access yahoo, waiting to retry...")
		    (sleep 5)
		    (setf count (- count 50))
		    nil))))))
