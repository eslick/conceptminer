;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: eventminer -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          local-search.lisp
;;;; Purpose:       Grab windows from documents already mined for specific queries
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  June 2006
;;;;

(in-package :conceptminer)

;;
;; Utilities
;;   

;;
;; The NEAR miner computes windows over the text, let's persist these
;;

;; Precompute important things like the location of the source & target
;; as well as a larger window

(defpclass training-window ()
  ((query :accessor training-window-query :initarg :query)
   (page :accessor training-window-page :initarg :page :index t)
   (direction :accessor training-window-direction :initarg :direction)
   (source-interval :accessor training-window-source-interval :initarg :source-interval)
   (target-interval :accessor training-window-target-interval :initarg :target-interval)
   target-phrase
   source-phrase
   training-phrase
   result
   (window-interval :accessor training-window-interval :initarg :window-interval)
   ;; Important parameters (for regression?)
   (width :accessor training-window-width :initarg :width :initform 20)
   (margin :accessor training-window-margin :initarg :margin :initform 10)
   ;; For analysis
   (filtered :accessor training-window-filtered :initarg :filtered :initform nil)
   (judgement :accessor training-window-judgement :initarg :judgement :initform nil)))

;;
;; Utility methods
;;

(defmethod print-object ((win training-window) stream)
  (format stream "#<TWIN: ~A ~A>" (training-window-query win) (training-window-page win)))

;; Simple interval data abstraction
(defun make-interval (start end) (cons start end))
(defun legal-interval-p (interval) (and (consp interval) (> (cdr interval) (car interval))))
(defun interval-start (interval) (car interval))
(defun interval-end (interval) (cdr interval))
(defun interval-size (interval) 
  (unless (legal-interval-p interval)
    (error "Illegal interval ~A"))
  (- (cdr interval) (car interval)))
(defun <-interval (position interval) (< position (car interval)))
(defun <=-interval (position interval) (<= position (car interval)))
(defun >-interval (position interval) (> position (cdr interval)))
(defun >=-interval (position interval) (>= position (cdr interval)))
(defun in-interval-p (position interval) (and (>= position (car interval))
					      (<= position (cdr interval))))

(defmacro destructure-interval-pair (vars pair &body body)
  `(let ((,(first vars)  (interval-start (first ,pair)))
	 (,(second vars) (interval-end (first ,pair)))
	 (,(third vars)  (interval-start (second ,pair)))
	 (,(fourth vars) (interval-end (second ,pair))))
     ,@body))

(defmethod interval-between-predicates ((win training-window))
  (if (eq (training-window-direction win) :forward)
      (- (interval-start (training-window-target-interval win))
	 (interval-end (training-window-source-interval win)))
      (- (interval-start (training-window-source-interval win))
	 (interval-end (training-window-target-interval win)))))

(defmethod training-window-phrase ((win training-window))
  (make-phrase-from-vdoc (page-contents (training-window-page win))
			 (window-start win) (- (window-end win) (window-start win))))

;; Window methods
(defmethod window-start ((win training-window))
  (interval-start (training-window-interval win)))

(defmethod window-end ((win training-window))
  (interval-end (training-window-interval win)))

(defmethod print-training-window ((win training-window) &key (with-tags nil) (stream t))
  (print-phrase (training-window-phrase win)
		:with-tags with-tags
		:stream stream))

(defmethod source-concept ((win training-window))
  (get-pconcept 
   (relation-source-id 
    (get-relation 
     (cnet-query-relation-id 
      (training-window-query win))))))

(defmethod target-concept ((win training-window))
  (get-pconcept 
   (relation-target-id 
    (get-relation 
     (cnet-query-relation-id 
      (training-window-query win))))))

(defmethod training-window-relation-type ((win training-window))
  (get-ptypename (relation-type-id (get-relation (cnet-query-relation-id (training-window-query win))))))
;;
;; Construction
;;

(defparameter *training-window-lookup-on-create* nil)

(defun make-training-window (query page source-interval target-interval width margin direction)
  (aif-ret (and *training-window-lookup-on-create*
		(find-training-window query page source-interval target-interval))
	   (compute-window-extent 
	    (make-instance 'training-window
			   :query query
			   :page page
			   :direction direction
			   :width width
			   :margin margin
			   :source-interval source-interval
			   :target-interval target-interval
			   :window-interval nil))))

(defmethod compute-window-extent (window &optional new-margin)
  (with-slots (direction source-interval target-interval margin page window-interval) window
    (when (and new-margin (not (= new-margin margin))) (setf margin new-margin))
    (let ((total-length (length-of (page-contents page))))
      (setf window-interval
	    (ecase direction
	      (:forward
	       (cons (-clamp (interval-start source-interval) margin 0)
		     (+clamp (interval-end target-interval) margin (1- total-length))))
	      (:backward 
	       (cons (-clamp (interval-start target-interval) margin 0)
		     (+clamp (interval-end source-interval) margin (1- total-length))))))))
  window)

(defmethod fucked-up-window-p ((window training-window))
  (declare (optimize (speed 3) (safety 1) (debug 1) (space 0)))
  (or (> (interval-start (training-window-interval window))
	 (interval-start (training-window-source-interval window)))
      (> (interval-start (training-window-interval window))
	 (interval-start (training-window-target-interval window)))
      (< (interval-end (training-window-interval window))
	 (interval-end (training-window-source-interval window)))
      (< (interval-end (training-window-interval window))
	 (interval-end (training-window-target-interval window)))
      (>= (interval-end (training-window-interval window)) 
	  (length-of (page-contents (training-window-page window))))))

(defun find-training-window (query page source-interval target-interval)
  "Find a training window that matches the provided parameters"
  (dolist (window (get-instances-by-value 'training-window 'page page))
    (when (and (equal source-interval (training-window-source-interval window))
	       (equal target-interval (training-window-target-interval window))
	       (eq query (training-window-query window)))
      (return-from find-training-window window))))

(defun training-window-equal (w1 w2)
    (and (equal (training-window-source-interval w1) (training-window-source-interval w2))
	 (equal (training-window-target-interval w1) (training-window-target-interval w2))
	 (eq (training-window-query w1) (training-window-query w2))))
  

;;
;; Extraction from pages & queries
;;

(defun extract-training-windows (query page width margin)
  "Extract a set of training windows from page according to the relation in query (cnet-query type)
   and where phrases are within width words of each other, including margin words on either end"
  (let ((relation (cnet-query-relation-id query)))
    (if (or (neq (type-of relation) 'fixnum) (eq relation 0))
	(warn "Relation from query ~A not a relation id or is 0" relation)
	(multiple-value-bind (forward-pairs backward-pairs)
	    (get-document-interval-pairs-for-phrases (page-contents page)
						     (relation-source-phrase relation)
						     (relation-target-phrase relation)
						     width)
	  (nconc (mapcar #'(lambda (interval-pair)
			     (make-training-window query page 
						   (first interval-pair) 
						   (second interval-pair) 
						   width margin :forward))
			 forward-pairs)
		 (mapcar #'(lambda (interval-pair)
			     (make-training-window query page 
						   (second interval-pair) 
						   (first interval-pair) 
						   width margin :backward))
			 backward-pairs))))))

(defun relation-source-phrase (relation-id)
  (pconcept-phrase (get-pconcept (relation-source-id (get-relation relation-id)))))

(defun relation-target-phrase (relation-id)
  (pconcept-phrase (get-pconcept (relation-target-id (get-relation relation-id)))))

(defun get-document-interval-pairs-for-phrases (vdoc phrase1 phrase2 width &key (concept-terms nil))
  "This returns a two sets of interval pairs as two-element lists.  Each interval
   pair consists of a source and target interval.  The first set are interval
   pairs of (phrase1 pharse2) and the second is the reverse, (phrase2 phrase1)"
  (cond ((null vdoc)
	 (values nil nil))
	((not (arrayp (document-text vdoc)))
	 (when (typep (document-text vdoc) 'vector-document)
	   (setf (document-text vdoc) nil)))
	(t (intervals-within-distance
	    (find-phrase-intervals phrase1 vdoc :match :words :lemma t :concept-terms concept-terms)
	    (find-phrase-intervals phrase2 vdoc :match :words :lemma t :concept-terms concept-terms)
	    width))))

(defun intervals-within-distance (list1 list2 width)
  "Take two lists of intervals as pairs of fixnums and identify intervals
   that have width or less distance (start2-end1) between them.
   Return two lists of fwd relations 1..2 and bck relations
   2...1 as two-element lists."
;;  (declare (optimize (speed 3) (space 0) (safety 1))
;	   (type fixnum width)
;	   (type list list1 list2))
  (labels ((start (it) (car it))
	   (end (it) (cdr it))
	   (iter (srclist dstlist fwd bck)
	     (if (or (null srclist) (null dstlist))
		 (values (nreverse fwd) (nreverse bck))
		 (let ((src (car srclist))
		       (dst (car dstlist)))
		   (declare (type cons src dst))
		   (let ((fwd-cmp (- (start dst) (end src)))
			 (bck-cmp (- (start src) (end dst))))
		     (declare (type fixnum fwd-comp bck-cmp))
		     (cond ((and (> fwd-cmp 0) (< fwd-cmp width))
			    (if (> (start src) (start dst))
				(iter srclist (cdr dstlist) (cons (list src dst) fwd) bck)
				(iter (cdr srclist) dstlist (cons (list src dst) fwd) bck)))
			   ((and (> bck-cmp 0) (< bck-cmp width))
			    (if (> (start src) (start dst))
				(iter srclist (cdr dstlist) fwd (cons (list dst src) bck))
				(iter (cdr srclist) dstlist fwd (cons (list dst src) bck))))
			   (t (if (> (start src) (start dst))
				  (iter srclist (cdr dstlist) fwd bck)
				  (iter (cdr srclist) dstlist fwd bck)))))))))
    (iter list1 list2 nil nil)))

;;
;; Get windows for queries
;;

(defmethod get-training-windows-for-page ((page page) &aux unique-windows)
  "Get training windows for a given page for all queries
   that map to that page"
  (let ((windows (get-instances-by-value 'training-window 'page page)))
    (flet ((find-dups (iwin) 
	     (remove-nulls
	      (loop for dwin in windows collect
		   (when (training-window-equal iwin dwin)
		     dwin)))))
      (loop while (not (null windows)) do
	   (let ((iwin (pop windows)))
	     (awhen (find-dups iwin)
	       (setf windows (set-difference windows it))
	       (drop-instances it))
	     (push iwin unique-windows)))))
  unique-windows)

(defmethod get-training-windows-for-query ((query cnet-query))
  "Get training windows for a given query"
  (select-if (lambda (window)
	       (eq (training-window-query window) query))
	     (remove-nulls 
	      (mapcan #'get-training-windows-for-page
		      (get-pages-for-query query)))))

(defun get-training-windows-for-relation-id (relation-id)
  "Get training windows for a single relation-id"
  (mapcan #'get-training-windows-for-query
	  (get-queries-for-relation relation-id)))

(defun get-training-windows-for-relation-ids (list)
  "Given a list of relation ids, return all the windows
   that have been mined from each page, for each query
   for each relation.  This is a big set!"
  (assert (get-relation (car list)))
  (mapcan #'get-training-windows-for-relation-id
	  list))

(defun get-training-windows-for-relation-type (type)
  "This is expensive.  Given a type, return all mined windows
   for this type"
  (when (symbolp type) (setf type (symbol-name type)))
  (get-training-windows-for-relation-ids 
   (cars (get-relations-for-type type))))

;;
;; Mining near queries from mined pages (for when we screw up doing this in real time)
;;

(defmacro decif (var &optional (amt 1))
  (when (listp var)
    (error "Can only perform conditional decrement on variables"))
  `(when ,var
     (decf ,var ,amt)))

(defmacro incif (var &optional (amt 1))
  (when (listp var)
    (error "Can only perform conditional increment on variables"))
  `(when ,var
     (incf ,var ,amt)))

(defun extract-windows-from-cached-documents (relation-ids &key window-count relation-count (width 20) (margin 10))
  (loop for rel in relation-ids
        when (nor (and window-count
		       (<= window-count 0))
		  (and relation-count
		       (<= (decif relation-count) 0))) nconcing
       (let ((queries (get-queries-for-relation rel)))
	 (loop for query in queries nconcing
	      (mapcan #'(lambda (page) 
			  (let ((windows (extract-training-windows query page width margin)))
			    (decif window-count (length windows))
			    windows))
		      (get-pages-for-query query))))))


