;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: eventminer -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          openmind-study.lisp
;;;; Purpose:       Code to support analyzing OpenMind content on the web
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  December 2005
;;;;

(in-package :conceptminer)

;;
;; Load OMCS Sentences into the DB
;;

(defun load-omcs-records (file &key limit (start 0) &aux (count 0))
  (with-open-file (stream file :direction :input)
    (awhile (read-line stream)
      (when (= (mod (incf count) 10000) 0)
	(format t "Total lines read: ~A~%" count))
      (when (and limit (<= (decf limit) 0))
	(port:gc)
	(return-from load-omcs-records t))
      (when (>= count start)
	(mvbind (id offset) (read-from-string it)
		(when (eq (type-of id) 'fixnum)
		  (unless (get-instances-by-value 'sentence-record 'id id)
		    (with-miner-transaction ()
		      (let ((query (make-sentence-query id (subseq it offset))))
			(index-query query))))))))))

;;
;; Sentence length distribution
;;

(defun compute-sentence-distribution ( count &optional (start 1))
  (let ((cary (make-array 100 :initial-element 0)))
    (loop for i from start upto (+ start count) do
	 (when (= (mod i 5000) 0)
	   (format t "Processed ~A records~%" i))
	 (let ((squery (get-instances-by-value 'sentence-record 'id i)))
	   (when squery
	     (let ((query-array (query-string (car squery))))
	       (awhen (and query-array (arrayp query-array)
			   (< (length query-array) 100))
		 (incf (aref cary (length query-array))))))))
    (remove-nulls
     (loop for j from 0 upto 99 collecting
	  (when (> (aref cary j) 0)
	    (list j (aref cary j) (coerce (/ (aref cary j) count) 'float)))))))

(defun percent-under-length-n (dist n &aux (total 0))
  (mapc (lambda (rec)
	  (when (<= (first rec) n)
	    (incf total (third rec))))
	dist)
  total)

;;
;; Simple hit rate filtered by sentence size
;;

(defun compute-sentence-hit-rate ( &key limit count &aux (counter 0))
  (let ((yes 0)
	(no 0))
    (catch 'exit-early
      (map-btree (lambda (oid srec)
		   (declare (ignore oid))
		   (when (and count (>= (incf counter) count))
		     (throw 'exit-early nil)) 
		   (when (or (not limit) (< (query-string-size srec) limit))
		     (if (get-instances-by-value 'page 'query srec)
			 (incf yes)
			 (incf no))))
		 (find-inverted-index 'sentence-record 'id)))
    `((:yes . ,yes) (:no . ,no))))

;;
;; Validate hit rates by searching inside pages
;;

;;
;; Average % of sites with mispellings 
;;

;;
;; Correlate mispellings with failed queries
;; 


;;
;; Useful queries
;;

(defun get-sentence-record (id)
  (car (get-instances-by-value 'sentence-record 'id id)))

(defun get-urls-for-sentence-id (id)
  (awhen (get-sentence-record id)
    (mapcar #'page-url 
	    (get-instances-by-value 'page 'query it))))

(defun get-pages-for-sentence-id (id)
  (awhen (get-sentence-record id)
    (get-instances-by-value 'page 'query it)))



;;
;; For each openmind sentence
;; - find pages containing variations on that sentence
;;   - modify surface expressions of verbs
;;   - correct mispelled words
;; - if not found, put in queue to analyze why (misspelling, etc)
;; 

;;
;; Literal queries
;;

(defcomponent omcs-literal-query-generator
  (:features :source)
  (:vars (current-id 0) max (skip 1))
  (:body
   (when (and max (<= (decf max) 0))
     (terminate))
   (awhen (get-instances-by-value 'sentence-record 'id (incf current-id skip))
     (if (> (length it) 1)
	 ;; NOTE: log duplicates here
	 (send (car it))
	 (send (car it))))))

(defcomponent printer
  (:vars (format-string "~A~%"))
  (:body
   (format t format-string data)))

(defcomponent query-string-printer
  (:body
   (format t "~A ~A~%" data (print-query data nil))
   (awhile (receive)
     (format t "~A ~A~%" it (print-query it nil)))))

(defcomponent page-printer
  (:vars (msg nil))
  (:body
   (flet ((print (it)
	    (format t "~A: ~A~%" (if msg msg "url") (page-url it))))
     (print data)
     (awhile (receive)
       (print it)))))

(defcomponent pass
  (:body
   (send data)))

;;
;; Test Containers
;;

(defcontainer omcs-test1
  (:children
   (gen omcs-literal-query-generator :max 10)
   (print query-string-printer))
  (:netlist
   (gen -> print)))

(defcontainer omcs-test2
  (:children
   (gen omcs-literal-query-generator :max 20)
   (print1 query-string-printer)
   (search url-search :searcher-class 'a9-searcher :urls 10 :threshold 5)
   (print2 page-printer))
  (:netlist
   (gen -> print1 search)
   (search -> print2)))

(defcontainer omcs-study2-test
  (:children
   (gen1 omcs-literal-query-generator :current-id 1 :skip 4 :max 10)
   (gen2 omcs-literal-query-generator :current-id 2 :skip 4 :max 10)
   (gen3 omcs-literal-query-generator :current-id 3 :skip 4 :max 10)
   (gen4 omcs-literal-query-generator :current-id 4 :skip 4 :max 10)
   (search1 pass :threaded t)
   (search2 pass :threaded t)
   (search3 pass :threaded t)
   (search4 pass :threaded t)
   (qprint query-string-printer :threaded t)
   (pprint printer :threaded t))
  (:netlist
   (gen1 -> search1 qprint)
   (gen2 -> search2 qprint)
   (gen3 -> search3 qprint)
   (gen4 -> search4 qprint)
   (search1 -> pprint)
   (search2 -> pprint)
   (search3 -> pprint)
   (search4 -> pprint)))

;;
;; Programs
;; 

(defcontainer omcs-study1
  (:children
   (gen omcs-literal-query-generator :current-id 1)
   (print1 query-string-printer)
   (search url-search :searcher-class 'a9-searcher :urls 10 :threshold 5)
   (print2 page-printer))
  (:netlist
   (gen -> print1 search)
   (search -> print2)))

(defcontainer omcs-study2
  (:children
   (gen1 omcs-literal-query-generator :current-id 1 :skip 4 :max 10)
   (gen2 omcs-literal-query-generator :current-id 2 :skip 4 :max 10)
   (gen3 omcs-literal-query-generator :current-id 3 :skip 4 :max 10)
   (gen4 omcs-literal-query-generator :current-id 4 :skip 4 :max 10)
   (qprint query-string-printer)
   (search1 url-search :searcher-class 'a9-searcher :urls 10 :threshold 8 :threaded t)
   (search2 url-search :searcher-class 'a9-searcher :urls 10 :threshold 8 :threaded t)
   (search3 url-search :searcher-class 'a9-searcher :urls 10 :threshold 8 :threaded t)
   (search4 url-search :searcher-class 'a9-searcher :urls 10 :threshold 8 :threaded t)
   (pprint page-printer :threshold 200 :threaded t)
   )
  (:netlist
   (gen1 -> search1 qprint)
   (gen2 -> search2 qprint)
   (gen3 -> search3 qprint)
   (gen4 -> search4 qprint)
   (search1 -> pprint)
   (search2 -> pprint)
   (search3 -> pprint)
   (search4 -> pprint)))



   


