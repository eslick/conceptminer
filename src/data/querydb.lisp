;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: eventminer -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          query.lisp
;;;; Purpose:       Persistent record of outgoing queries
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  February 2006
;;;;

(in-package :conceptminer)

;;
;; Miner query records
;; 

(defpclass query ()
  ((phrase :accessor query-phrase :initarg :query-phrase)))

(defmethod query-string ((q query))
  (assert (subtypep (query-phrase q) 'array))
  (token-array->string (query-phrase q)))

(defmethod query-string-size ((q query))
  (length (query-string q)))

;; query types = :literal :concept :relation

(defun make-query (query-phrase)
  (make-instance 'query
		 :query-phrase (ensure-token-array query-phrase)))

(defun ensure-token-array (phrase)
  (etypecase phrase
    (string (setf phrase (string->token-array phrase)))
    (list nil)
    (array nil))
  (assert (> (length phrase) 0))
  phrase)

(defmethod ensure-query-string (searcher (query query))
  (let ((query-phrase (query-phrase query)))
    (when (quoted-phrases-p searcher)
      (etypecase query-phrase
	(array (strlist->quoted-substrings
		(map-across #'token-for-id query-phrase)))
	(list (strlist->quoted-substrings query-phrase))
	(string (make-quoted-query query-phrase))))))

(defparameter *query-string-print-width* 4)

(defmethod print-object ((obj query) stream)
  (format stream "#<QUERY ~A>" 
	  (append 
	   (map-across #'token-for-id (subseq (query-string obj) 0
					      (min (length (query-string obj))
						   *query-string-print-width*))))
	   (if (> (length (query-string obj))
		  *query-string-print-width*)
	       (list "...")
	       nil)))

(defmethod print-query ((q query) &optional (stream t))
  (format stream "~A~%" (apply #'concatenate 'string 
			(shuffle (map-across #'token-for-id (query-string q))
				 (repeat " " (1- (length (query-string q))))))))

;;
;; Sentence queries for OMCS study
;;
	  
(defpclass sentence-record (query)
  ((id :type fixnum :accessor sentence-record-id
       :initarg :id :index t
       :documentation "The OMCS sentence ID")))

(defun make-sentence-query (id query)
  (make-instance 'sentence-record
		 :id id
		 :query-phrase (ensure-token-array query)))

(defmethod print-object ((obj sentence-record) stream)
  (format stream "#<SENTENCE ~A: ~A>" 
	  (sentence-record-id obj)
	  (append 
	   (map-across #'token-for-id (subseq (query-string obj) 0
					      (min (length (query-string obj))
						   *query-string-print-width*)))
	   (if (> (length (query-string obj))
		  *query-string-print-width*)
	       (list "...")
	       nil))))


;;
;; Relation queries for CNET study
;;

(defpclass cnet-query (query)
  ((rel :type fixnum :accessor cnet-query-relation-id
	:initarg :rid :index t)
   (type :accessor cnet-query-type :initarg :type)))

(defun make-cnet-query (relation-id query type)
  (let ((query-rep (ensure-token-array query)))
    (awhen (get-instances-by-value 'cnet-query 'rel relation-id)
      (loop for query in it do
	   (when (and (equal type (cnet-query-type query))
		      (equalp query-rep (query-phrase query)))
	     (return-from make-cnet-query query))))
    (make-instance 'cnet-query
		   :rid relation-id
		   :type type
		   :query-phrase query-rep)))

(defmethod query-source-phrase ((query cnet-query))
  (first (query-phrase query)))

(defmethod query-target-phrase ((query cnet-query))
  (second (query-phrase query)))

(defmethod get-cnet-query (id)
  (get-instance-by-value 'cnet-query 'rel id))

(defmethod query-string ((q cnet-query))
  (assert (and (listp (query-phrase q))
	       (subtypep (type-of (query-source-phrase q)) 'array)
	       (subtypep (type-of (query-target-phrase q)) 'array)))
  (string-downcase 
   (token-array->string
    (concatenate '(vector fixnum) 
		 (first (query-phrase q))
		 (make-array 1 :element-type 'fixnum :initial-element (id-for-token "AND"))
		 (second (query-phrase q))))))

(defmethod print-query ((q cnet-query) &optional (stream t))
  (let ((str (query-string q)))
    (format stream "~A~%" str)))

(defmethod print-object ((obj cnet-query) stream)
  (format stream "#<CNET-QUERY ~A: ~A>" 
	  (cnet-query-relation-id obj)
	  (mapcar 
	   (lambda (ta) 
	     (concatenate 'string "\"" (token-array->string ta) "\""))
	   (query-phrase obj))))