;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: eventminer -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          relation-study.lisp
;;;; Purpose:       Code to support analyzing Conceptnet data
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  April 2006
;;;;

(in-package :conceptminer)

;; Components all pass collections and add/remove data via accessors
;; that can be passed as parameters.  (:input-fn obj) (setf (:output-fn obj) data)


;;
;; Query generation
;;

(defcomponent cnet-relation-generator
  (:features :source)
  (:vars (start 1) current end (cnet *cnet-graph*) type)
  (:initialize
   (setf current start)
   (unless (numberp type)
     (setf type (get-ptype type))))
  (:body
   (awhen (get-relation current cnet)
     (when (or (not type) (= type (relation-type-id it)))
;;       (format t "sending: ~A -> ~A~%" current it)
       (send (cons current it))))
   (incf current)
   (when (and end (>= current end))
     (terminate))))

(defcomponent cnet-query-generator
  (:body
;;   (format t "~A~%" data)
   (dolist (query (generate-relation-queries data))
     (send query))))

(defun generate-relation-queries (relation-rec)
  "Take a relation id and produce a set of cnet-queries for all
   pairs of surface forms of the two concepts"
  (remove-nulls 
;; FOR NON 'NEAR' QUERIES
;;   (mappend (lambda (pair)
;;	      (mapcar (lambda (type)
;;			(make-cnet-query (car relation-rec) pair type))
;;		      '(:and :window)))
;;	    (relation-surface-pairs (cdr relation-rec)))))
;;   (mappend (lambda (pair)
   (mapcar (lambda (pair)
	     (make-cnet-query (car relation-rec) pair :window))
	   (relation-surface-pairs (cdr relation-rec)))))

(defcomponent send-fn-results
  (:vars fn (marker-type :end-of-results))
  (:body
   (dolist (result (funcall (etypecase fn
			      (symbol (symbol-function fn))
			      (function fn))
			    data))
     (send result))
   (send (make-instance 'pcomp-data-marker :type marker-type :data data))))

;;
;; Generate queries by type
;;

(defmethod ensure-query-string (searcher (query cnet-query))
  "Override default query string generator for searchers to 
   differentiate by type of cnet query"
  (generate-relation-query-string (query-phrase query) 
				  (quoted-phrases-p searcher)
				  (cnet-query-type query)))

(defparameter *star-count* 10)

(defun generate-relation-query-string (pair quoted-p type)
  "Generate a printable version of the provided surface form token arrays.
   Supports 'and' and by N '*' queries"
  (let ((quote (id-for-token "\""))
	(star (id-for-token "*")))
    (string-downcase 
     (token-array->string
      (if quoted-p    
	  (case type
	    (:and (concatenate '(vector fixnum) `(,quote) (first pair) `(,quote ,quote) (second pair) `(,quote)))
	    (:window (concatenate '(vector fixnum) 
				  `(,quote)
				  (first pair) 
				  `(,quote)
				  (repeat star *star-count*) ;; (- 10 (length (first pair)) (length (second pair))))
				  `(,quote)
				  (second pair)
				  `(,quote)))
	    (t nil))
	  (case type
	    (:and (concatenate '(vector fixnum) (first pair) (second pair)))
	    (:window (concatenate '(vector fixnum)
				  (first pair) 
				  (repeat star *star-count*) ;; (- 10 (length (first pair)) (length (second pair))))
				  (second pair)))))))))

;;
;; Utility components
;;


(defcomponent updater
  (:vars (update-fn nil))
  (:body 
   (when update-fn
     (funcall update-fn data))))

(defcomponent delay
  (:vars (amount 0.1))
  (:body
   (sleep 0.1)))

(defcomponent list-accumulator
  (:vars (write-to '*accum-result*) op (print nil))
  (:body
   (when print (format t "writing: ~A~%" data))
   (push data (symbol-value write-to))))

(defcomponent list-generator
  (:features :source)
  (:vars list rate (current 0) manual (tick t))
  (:initialize 
   (assert (not (null list))))
  (:tick
   (setf tick t)
   (format t "Tick ~A~%" (pcomp::name self))
   (setf (pcomp::component-source-p self) t))
  (:body
   (when (or (not manual) (and manual tick))
     (when (or (not rate) (= current 0))
       (if (null list)
	   (terminate)
	   (send (pop list))))
     (when rate
       (setf current (mod (1+ current) rate)))
     (when (and manual tick)
       (setf tick nil)
       (setf (pcomp::component-source-p self) nil)))))

(defcomponent filter
  (:vars filter-fn)
  (:initialize
   (assert (symbol-function filter-fn)))
  (:body
   (labels ((send-if-not (datum)
	      (unless (or (pcomp-marker-p datum)
			  (funcall (symbol-function filter-fn) datum))
		(send datum))))
     (send-if-not data)
     (awhile (receive)
       (send-if-not it)))))

;;
;; Filtering
;;

(defvar *url-kill-list*
  '("http://www.cs.caltech.edu/~sidd/"
    "http://pedia.media.mit.edu/wiki/"
    "http://www.eturner.net/omcsnetcpp/"
    "http://www.conceptnet.org/"))

(defun page-filter (page)
  (or (already-have? page)
      (blacklisted-url? page)))

(defun already-have? (page)
  (let ((pages (lookup-pages-by-url (page-url page))))
    (when pages
      (page-indexed-p (car pages)))))

(defun blacklisted-url? (page)
  (member (page-url page) *url-kill-list* :test #'(lambda (x y) 
				  (equalp y (subseq x 0 (min (length x) (length y)))))))

  


