;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: eventminer -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          inverseindex.lisp
;;;; Purpose:       Quick and dirty inverse word index in elephant for proximity queries
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  February 2006
;;;;

(in-package :conceptminer)

;; inverse-index [root: btree]   [token -> token-index]
;; token-index [btree: btree]    [page -> locationset]
;;             [size: fixnum]
;;             [frequency: float]


(defclass inverse-index ()
  ((root :accessor inverse-index-root :initarg :root :initform (make-btree))
   (lemmatize :accessor inverse-index-lemmatize-p :initarg :lemma :initform nil)
   (remove-stopwords :accessor inverse-index-remove-stopwords-p :initarg :stopwords :initform t))
  (:documentation "Top level standard class to handle inverse indexing of a given set of
                   elements such as text tokens.  This can work over combined sets in a 
                   common namespace or for multiple domains you can use multiple instances"))

(defmethod get-con ((idx inverse-index))
  "Simple way to get the base controller"
  (get-con (inverse-index-root idx)))

(defmethod get-value (token (idx inverse-index))
  (get-value token (inverse-index-root idx)))

(defmethod (setf get-value) (token-index token (idx inverse-index))
  (setf (get-value token (inverse-index-root idx)) token-index))

(defmethod make-cursor ((idx inverse-index))
  (make-cursor (inverse-index-root idx)))

;;
;; Add a subindex for a particular domain token
;;

(defmethod add-token-index ((idx inverse-index) token)
  (setf (get-value token idx) (make-token-index)))

;;
;; Top level document indexing
;;

(defmethod index-document ((idx inverse-index) (page page))
  (assert (eq (find-class 'vector-document) (class-of (page-contents page))))
  (when (and (slot-boundp page 'page-indexed) (page-indexed-p page))
    (warn "Page already indexed, ignoring"))
  (index-by-tokens idx (document-text (page-contents page)) page)
  (setf (page-indexed-p page) t))

(defmethod index-by-tokens ((idx inverse-index) (array array) (target persistent))
  (with-slots (lemmatize remove-stopwords) idx
    (let ((locations (make-hash-table :size 1000)))
      (loop for token across array 
	    for offset from 0 do
	   (when (not (and remove-stopwords (stopword? token)))
	     (when lemmatize
	       (setq token (get-lemma-for-id token)))
	     (push offset (gethash token locations))))
      ;; This should speed up writes by turning off auto-commit & avoiding synchs
;;      (with-miner-transaction (:txn-nosync t :dirty-read t)
      (loop for location-list being each hash-value using (hash-key token) of locations do
	   (add-token-reference idx target token (make-instance 'location-set :initial-contents (nreverse location-list)))))))

(defmethod unindex-document ((idx inverse-index) (page page) &key (contents-changed nil))
  "This presumes that the page contents haven't changed and
   will provide a fast key into all the inverse-indices that
   need to be removed"
  (if (and (not contents-changed) (page-indexed-p page))
      (progn 
	(unindex-by-tokens idx (document-text (page-contents page)))
	(setf (page-indexed-p page) nil))
      (unindex-document-by-page-identity idx page)))

(defmethod unindex-by-tokens ((idx inverse-index) (array array))
  (with-slots (lemmatize remove-stopwords) idx
    (with-miner-transaction (:txn-nosync t)
      (loop for token across array
	 when (not (and remove-stopwords (stopword? token))) do
	   (let ((set
		  (if lemmatize
		      (remove-location-set (inverse-index-root idx) (get-lemma-for-id token))
		      (remove-location-set (inverse-index-root idx) token))))
	     (when set
	       (slot-makunbound set 'array)
	       (slot-makunbound set 'factor)))))))

(defun remove-location-set (root token)
  (let ((set (get-value token root)))
    (setf (get-value token root) nil)
    set))

(defun unindex-document-by-page-identity (idx page)
  "This is an expensive operation which requires a search for
   indexed page oids through all the inverse indices, or one
   btree lookup per indexed word which could be tens of thousands
   of lookups"
  (with-miner-transaction ()
    (with-btree-cursor (cur (inverse-index-root idx))
      (loop 
	 (multiple-value-bind (valid? token token-index) (cursor-next cur)
	   (declare (ignore token))
	   (unless valid?
	     (return nil))
	   (when (get-value page token-index)
	     (remove-kv page token-index)))))))

(defmethod make-inverse-index-cursor ((idx inverse-index) token)
  (awhen (get-value token idx)
    (make-cursor it)))

;;
;; Add a page an ordered location set for all locations of the domain token
;; within the page
;;

;; (defmethod add-token-reference ((idx inverse-index) (page page) token location)
;;   (let ((token-index (get-value token idx)))
;;     (when (not token-index)
;;       (setq token-index (add-token-index idx token)))
;;     (aif (get-value page token-index)
;; 	 (setf (get-value page token-index)
;; 	       (insert-location it location))
;;       (setf (get-value page token-index)
;; 	    (make-instance 'location-set :initial-contents location)))))

(defmethod add-token-reference ((idx inverse-index) target token (ls location-set))
  (let ((token-index (get-value token idx)))
    (when (not token-index)
      (setq token-index (add-token-index idx token)))
    (when (get-value target token-index)
      (warn "Cannot override an existing location set without receiving this warning!"))
    (with-miner-transaction (:txn-nosync t)
      (setf (get-value target token-index) ls)
      (incf (token-index-size token-index))
      (update-average-frequency token-index ls))))

;;
;; Simple Token Index class, keeps statistics for query optimization
;;

(defpclass token-index ()
  ((btree :accessor token-index-btree :initarg :btree :initform (make-btree))
   (size :accessor token-index-size :initarg :size :initform 0)
   (frequency :accessor token-index-frequency :initform (cons 0 1.0))))

(defmethod print-object ((obj token-index) stream)
  (format stream "#<TIDX sz: ~A fq: ~A>" (token-index-size obj) (cdr (token-index-frequency obj))))

(defun make-token-index ()
  (make-instance 'token-index))

(defmethod get-value (page (tidx token-index))
  (get-value page (token-index-btree tidx)))

(defmethod (setf get-value) (set page (tidx token-index))
  (setf (get-value page (token-index-btree tidx)) set))

(defmethod make-cursor ((tidx token-index))
  (make-cursor (token-index-btree tidx)))

(defmethod update-average-frequency ((token-idx token-index) (set location-set))
  "Frequency is a cons consisting of (count . avg-length)"
  (destructuring-bind (count . avg) (token-index-frequency token-idx)
    (setf (token-index-frequency token-idx)
	  (cons (1+ count)
		(/ (+ (* count avg) 
		      (length (location-set-array set)))
		   (1+ count))))))

    
    

