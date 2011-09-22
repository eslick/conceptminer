;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: eventminer -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          relations.lisp
;;;; Purpose:       Code to represent Conceptnet data persistently
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  April 2006
;;;;

(in-package :conceptminer)

;; phrase: vector of token ids
;; relations: vector-list of relation record ids
(defpclass pconcept ()
  ((id :accessor pconcept-id :initarg :id :index t)
   (phrase :accessor pconcept-phrase :initarg :phrase :index t)
   (rels :accessor pconcept-relations :initarg :relations)))

(defmethod pconcept-p ((c pconcept)) t)
(defmethod pconcept-p ((obj t)) nil)

(defmethod print-object ((object pconcept) stream)
  (let ((str (pconcept-string object)))
    (format stream "#<PCONCEPT \"~A\">"
	    (subseq str 0 (min 20 (length str))))))

(defmethod pconcept-string ((object pconcept))
  (token-array->string (pconcept-phrase object)))

(defvar *cnet-graph* nil
  "Default system graph object for easier user api")

(defpclass cnet-graph ()
  ((typemap :accessor cnet-stored-types :initarg :typemap :initform (make-hash-table :test 'equalp)
	    :documentation "Maintain typename to id mapping")
   (typemap-cache :transient t)
   (tcounter :accessor cnet-type-counter :initarg :tcounter :initform 0)
   (relations :accessor cnet-relation-map :initarg :relations :initform (make-btree)
	      :documentation "Map relation id's to relation records")
   (rcounter :accessor cnet-relation-counter :initarg :rcounter :initform 0)
   (ccounter :accessor cnet-pconcept-counter :initarg :ccounter :initform 0)))

;;
;; Relation type management
;;

(defmethod cnet-type-map ((cnet cnet-graph))
  (let ((val (and (slot-boundp cnet 'typemap-cache)
		  (slot-value cnet 'typemap-cache))))
    (if val val
	(setf (slot-value cnet 'typemap-cache) 
	      (cnet-stored-types cnet)))))

(defun force-ptype (name id &optional (cnet *cnet-graph*))
  "Force a given id-name pair in the type map, warn if already mapped
   as we might be invalidating something" 
  (let ((cmap (cnet-type-map cnet)))
    (when (gethash name cmap)
      (warn "Remapping type name ~A from ~A to ~A" name
	    (gethash name cmap) id))
    (setf (gethash name cmap) id)
    (when (> id (cnet-type-counter cnet))
      (setf (cnet-type-counter cnet) id))
    (setf (cnet-stored-types cnet) cmap)
    id))

(defun find-ptype (name &optional (cnet *cnet-graph*))
  (typecase name
    (number name)
    (string (gethash (string-downcase name) (cnet-type-map cnet)))))

(defun get-ptype (name &optional (cnet *cnet-graph*))
  (if (numberp name)
    name
    (aif-ret (gethash name (cnet-type-map cnet))
	     (force-ptype name (incf (cnet-type-counter cnet)) cnet))))

(defun get-ptypename (tid &optional (cnet *cnet-graph*))
  (string-downcase 
   (if (stringp tid)
       tid
       (maphash (lambda (name id)
		  (when (eq id tid)
		    (return-from get-ptypename name)))
		(cnet-type-map cnet)))))

;;
;; Persistent concept management
;;

(defun make-pconcept (cnet-graph tokarry &key id (errorp t))
  (assert (or (not errorp) 
	      (and (arrayp tokarry)
		   (not (get-instance-by-value 'pconcept 'phrase tokarry)))))
  (when (and id (> id (cnet-pconcept-counter cnet-graph)))
    (setf (cnet-pconcept-counter cnet-graph) id))
  (make-instance 'pconcept 
		 :id (aif-ret id (incf (cnet-pconcept-counter cnet-graph)))
		 :phrase tokarry
		 :relations (make-vector-list :initial-size 3)))

(defun find-pconcept (conceptref &optional (cnet *cnet-graph*))
  (declare (ignore cnet))
  (get-instance-by-value 'pconcept 'phrase 
			 (etypecase conceptref
			   (pconcept 
			    (let ((arry (pconcept-phrase conceptref)))
			      (if (or (adjustable-array-p arry) (not (eq (array-element-type arry) 'fixnum)))
				  (make-array (length arry) :element-type 'fixnum :initial-contents arry)
				  arry)))
			   (string (string->token-array conceptref))
			   (array (make-array (length conceptref) :element-type 'fixnum :initial-contents conceptref)))))

(defun get-pconcept (concept-ref &optional (cnet *cnet-graph*))
  "Get a concept by various methods.  Creating it if necessary."
  (if (subtypep (type-of concept-ref) 'pconcept)
      concept-ref
      (let ((tokarry (pconcept-ref-as-token-array concept-ref)))
	(aif-ret (aif (numberp concept-ref)
		      (get-instance-by-value 'pconcept 'id concept-ref)
		      (get-instance-by-value 'pconcept 'phrase tokarry))
		 (make-pconcept cnet tokarry)))))

(defun pconcept-ref-as-token-array (concept-ref)
  (etypecase concept-ref
    (number (awhen (get-instance-by-value 'pconcept 'id concept-ref)
	      (pconcept-phrase it)))
    (string (string->token-array concept-ref))
    (array concept-ref)
    (phrase (get-instance-by-value 'pconcept 'phrase (phrase->token-array concept-ref)))))

;;
;; Low space cost relation rep
;; 

(defmethod add-relation ((cnet cnet-graph) typeref src-ref dst-ref origin)
  (let ((id (get-unique-relation-id cnet))
	(src (get-pconcept src-ref cnet))
	(dst (get-pconcept dst-ref cnet)))
    (setf (pconcept-relations src)
      (if (and (slot-boundp src 'rels)
	       (eq (type-of (pconcept-relations src)) 'vlist))
	  (add id (pconcept-relations src))
	  (add id (make-vector-list :initial-size 3 :initial-element 0 :type 'fixnum :unique-p t))))
    (setf (get-value id (cnet-relation-map cnet))
	  (make-relation (get-ptype typeref cnet)
			 (pconcept-id src)
			 (pconcept-id dst)
			 (cond ((eq (type-of origin) 'sentence-record)
				(sentence-record-id origin))
			       ((numberp origin)
				origin)
			       ((null origin)
				0)
			       (t (error "Invalid origin specified")))))
    id))

(defun make-relation (typeid srcid dstid originid)
  (make-array 4 
	      :element-type 'fixnum 
	      :initial-contents (list typeid srcid dstid originid)
	      :adjustable nil
	      :fill-pointer nil))

(defun relation-type-id (relation)
  (assert (and (arrayp relation) (eq (length relation) 4)))
  (aref relation 0))

(defun relation-source-id (relation)
  (assert (and (arrayp relation) (eq (length relation) 4)))
  (aref relation 1))

(defun relation-target-id (relation)
  (assert (and (arrayp relation) (eq (length relation) 4)))
  (aref relation 2))

(defun relation-origin-id (relation)
  (assert (and (arrayp relation) (eq (length relation) 4)))
  (aref relation 3))

(defmethod get-unique-relation-id ((cnet cnet-graph))
  (incf (cnet-relation-counter cnet)))

;;
;; Various CNET data accessors
;;

(defun get-relation (id &optional (cnet *cnet-graph*))
  (get-value id (cnet-relation-map cnet)))

(defun find-relation (relation c1 c2)
  (let ((type-id (get-ptype relation))
	(targ-id (pconcept-id (find-pconcept c2))))
    (select-if (lambda (relation)
		 (and (eq targ-id (relation-target-id relation))
		      (eq type-id (relation-type-id relation))))
	       (get-relation-ids-for-concept c1)
	       :key #'get-relation)))

(defun connected-pconcept (pc)
  (pconcept-relations pc))

;;(defmethod pconcept-string 

(defun print-relation-id (rid &optional (stream t))
  (print-relation (get-relation rid) stream))

(defun print-relation (relation &optional (stream t))
  (flet ((pc-string (id)
	   (nstring-downcase
	    (token-array->string 
	     (pconcept-phrase
	      (get-pconcept id))))))
    (assert (arrayp relation))
    (format stream "(~A \"~A\" \"~A\")~%"
	    (get-ptypename (relation-type-id relation))
	    (pc-string (relation-source-id relation))
	    (pc-string (relation-target-id relation)))))

(defun get-relations-for-concept (concept &optional (cnet *cnet-graph*))
  (map-across (lambda (id)
		(get-relation id cnet))
	      (vlist-array 
	       (pconcept-relations 
		(get-pconcept concept cnet)))))

(defun get-relation-ids-for-concept (concept &optional (cnet *cnet-graph*))
  (vlist->list 
   (pconcept-relations 
    (find-pconcept concept cnet))))

(defun get-relation-ids-for-concept-by-type-id (concept type-id &optional (cnet *cnet-graph*) &aux rels)
  (handler-case
      (map-across (lambda (rid)
		    (let ((rel (get-relation rid)))
		      (when (eq (relation-type-id rel) type-id)
			(push rid rels))))
		  (handler-bind
		      ((error #'(lambda (c) (declare (ignore c)) nil)))
		    (vlist-array
		     (pconcept-relations (case concept
					   (pconcept concept)
					   (t (get-pconcept concept cnet)))))))
    (unbound-slot () nil)
    (t () nil))
  rels)

(defun get-relations-for-type (typeref)
  (get-relations-for-type-over-range typeref 1 (1- (cnet-relation-counter *cnet-graph*))))

(defun get-relations-for-type-over-range (typeref start end &aux list)
  (when (stringp typeref)
    (setf typeref (get-ptype typeref)))
  (loop for rid from start upto end do
       (let ((relation (get-relation rid)))
	 (when (and relation (= typeref (relation-type-id relation)))
	   (push (cons rid relation) list))))
  list)



;; ==========================================
;; Simple queries and index management
;; ==========================================

(defun get-queries-for-relation (id)
  (get-instances-by-value 'cnet-query 'rel id))

(defun get-pages-for-query (query)
  (get-associations query *conceptminer-query-to-page-map*))

(defun get-pages-for-relation (id)
  (mapcan #'get-pages-for-query
	  (get-queries-for-relation id)))

(defmethod get-queries-for-page ((page page))
  (get-inverse-associations page *conceptminer-query-to-page-map*))

(defun drop-page (page)
  "Need to cleanup associations"
  (when (page-indexed-p page)
    (unindex-document *conceptminer-inverse-page-index* page))
  (let ((queries (get-queries-for-page page)))
    (when queries
      (dolist (query queries)
	(drop-association query page *conceptminer-query-to-page-map*))))
  (drop-instances (mklist page)))

(defun drop-pages (pages)
  (mapc #'drop-page pages)
  t)

(defun drop-query (query)
  "Make sure query is no longer indexed and any associations tied to it are gone"
  (drop *conceptminer-query-to-page-map* query)
  (drop-instances (mklist query)))

;; ========================================
;; Initialization
;; ========================================

(defun load-cnet-relations (predicate-file cnet &key limit (start 0) &aux (count 0))
  (with-open-file (stream predicate-file :direction :input)
    (awhile (read-line stream nil)
      (when (= (mod (incf count) 10000) 0)
	(format t "Total lines read: ~A~%" count))
      (when (and limit (<= (decf limit) 0))
	(port:gc)
	(return-from load-cnet-relations t))
      (when (>= count start)
	(case (aref it 0)
	  (#\C (fastload-concept it cnet))
	  (#\T (fastload-type it cnet))
	  (#\L (fastload-relation it cnet)))))))

(defparameter *id-len* 8)
(defparameter *type-len* 4)

(defun fastload-concept (line cnet)
  (let ((id (utils::parse-integer line :start 1 :end (+ 1 *id-len*))))
    (drop-instances (get-instances-by-value 'pconcept 'id id))
    (make-pconcept cnet (string->token-array (subseq line (+ 2 *id-len*))) :id id :errorp nil)))

(defun fastload-type (line cnet)
  (let ((id (utils::parse-integer line :start 1 :end (+ 1 *type-len*))))
    (force-ptype (subseq line (+ 2 *type-len*)) id cnet)))

(defun fastload-relation (line cnet)
  (let ((srcid (utils::parse-integer line :start 1 :end (+ 1 *id-len*)))
	(tid (utils::parse-integer line :start (+ 2 *id-len*) :end (+ 2 *id-len* *type-len*)))
	(dstid (utils::parse-integer line :start (+ 3 *id-len* *type-len*))))
    (add-relation cnet tid srcid dstid nil)))

(defun tokenize-pconcepts (maxid &aux (count 0))
  (loop for i from 1 upto maxid do
       (when (eq (mod (incf count) 10000) 0)
	 (format t "~A~%" count))
       (let ((inst (get-instance-by-value 'pconcept 'id i)))
	 (when (and inst (stringp (pconcept-phrase inst)))
	   (setf (pconcept-phrase inst) (string->token-array (pconcept-phrase inst)))))))