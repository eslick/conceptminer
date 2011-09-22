;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: eventminer -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          fetch.lisp
;;;; Purpose:       Persistent record of outgoing queries
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  February 2006
;;;;

(in-package :conceptminer)

(defmethod ensure-query-string (searcher (s string))
  (declare (ignore searcher))
  s)

(defmethod ensure-query-string (searcher (a array))
  (declare (ignore searcher))
  (token-array->string a))

(defcomponent url-search
    ;; Input: a query string
    ;; Output: page records with valid urls (via multiple calls to send-data)
    ;; Config: searcher - an object implementing search API 
    ;;         urls - The number of urls to fetch
    ;;         mode - Fetch a :total of 'urls' or 'urls' count of :new urls
    (:vars (urls 10) (mode :total) (searcher nil) searcher-class (searcher-initargs nil)
	   (association *conceptminer-query-to-page-map*) (always-associate nil))
    (:initialize (assert searcher-class)
		 (setf searcher (apply #'make-instance searcher-class searcher-initargs)))
    (:body 
     (assert searcher)
     (when (need-more-urls? data urls mode association)
       (format t "Searching for: ~A~%" (if (eq (type-of data) 'array) (token-array->string data) data))
       (mapcar (lambda (url)
		 (aif (lookup-pages-by-url url)
		      (progn
			(when always-associate
			  (add-association data (car it) association))
			(send (car it)))
		      (let ((page (make-page url)))
			(add-association data page association)
			(send page))))
	       (handler-case
		   (do-search searcher (ensure-query-string searcher data)
			      :urls urls)
		 (EXCL:SOCKET-ERROR ()
		   (awhen (find-restart 'retry)
		     (invoke-restart it))))))))

(defcomponent url-fn-search
    ;; Input: a query
    ;; Output: page records with valid urls (via multiple calls to send-data)
    ;; Config: searcher - an object implementing search API 
    ;;         urls - The number of urls to fetch
    ;;         mode - Fetch a :total of 'urls' or 'urls' count of :new urls
    (:vars (urls 10) (mode :total) (searcher nil) searcher-class (searcher-initargs nil)
	   (fn nil) (association nil))
    (:initialize (assert searcher-class)
		 (setf searcher (apply #'make-instance searcher-class searcher-initargs))
		 (setf (searcher-max-refs searcher) urls))
    (:body 
     (assert searcher)
     (if (need-more-urls? data urls mode association)
	 (progn 
	   (format t "Searching for: ~A~%" data)
	   (mapcar (lambda (url)
		     (aif (lookup-pages-by-url url)
			  (let ((page (car it)))
			    (awhen (get-associations-as-oids data association)
			      (unless (find-value it (elephant::oid page))
				(add-association data page association)))
			    (send page))
			  (let ((page (make-page url)))
			    (add-association data page association)
			    (send page))))
		   (handler-case
		       (let ((found-urls (funcall (symbol-function fn) searcher data)))
			 (when found-urls (format t "Found ~A urls~%" (length found-urls)))
			 found-urls)
		     (EXCL:SOCKET-ERROR ()
		       (awhen (find-restart 'retry)
			 (invoke-restart it))))))
	 (dolist (page (get-associations data association))
	   (send page)))))
;;	 (format t "Skipping: ~A~%" data))))

(defun query-page-count (query association)
  (aif (get-associations-as-oids query association)
       (length-of it)
       0))

(defun need-more-urls? (query urls mode association)
  (declare (ignorable urls mode))
;;  t)
  (if (> (query-page-count query association) 0)
      nil t))
;;  (or (eq mode :new)
;;      (and (eq mode :total) (< (query-page-count query) (* 0.9 urls)))))

(defun make-quoted-query (string)
  (format nil "\"~A\"" string))


  
  
