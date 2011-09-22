;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: eventminer -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          odb.lisp
;;;; Purpose:       The top level interface to our object store
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  February 2006
;;;;
;;;; NOTES:

(in-package :conceptminer)

;; (defvar *conceptminer-instance* nil)

;; (defclass conceptminer ()
;;   ((db :accessor conceptminer-db :initarg :db)
;;    (db-path :accessor conceptminer-db-path :initarg :db-path)
;;    (inv-page-index :accessor conceptminer-inv-page-index :initarg :page-idx)
;;    (inv-query-index :accessor conceptminer-inv-query-index :initarg :query-idx)
;;    (query-page-map :accessor conceptminer-query-to-page-map :initarg :q2p-map)))

(defvar *conceptminer-db* nil)
(defvar *conceptminer-db-path* '(:bdb "/Users/eslick/Work/db/conceptminer/"))

(defun get-from-miner-root (sym)
  (get-from-root sym :store-controller *conceptminer-db*))

(defun add-to-miner-root (sym value)
  (add-to-root sym value :store-controller *conceptminer-db*))

;;
;; Indexing and mapping
;;

(defvar *conceptminer-inverse-page-index* nil)
(defvar *conceptminer-inverse-query-index* nil)
(defvar *conceptminer-query-to-page-map* nil)
(defvar *conceptminer-instance-query-to-page-map* nil)
(defvar *conceptminer-instance-to-pattern-map* nil)
(defvar *conceptminer-instance-to-page-map* nil)

(defun index-query (query)
  (assert (arrayp (query-string query)))
  (index-by-tokens *conceptminer-inverse-query-index* 
		   (query-string query)
		   query))

(defun lookup-queries (search-string)
  (query-inverse-index *conceptminer-inverse-query-index* search-string))

(defun index-page (page)
  (index-document *conceptminer-inverse-page-index* page))

(defun lookup-pages (search-string)
  (query-inverse-index *conceptminer-inverse-page-index* search-string))
  

;;
;; Helpful macros for dealing with transactionalism
;;

(defmacro with-miner-store (args &body body)
  "Sanity check any operations over the store"
  (declare (ignore args))
  `(progn
     (assert *conceptminer-db*)
     (let ((*old-store* *store-controller*)
	   (*store-controller* *conceptminer-db*))
       (declare (special *store-controller*)
		(dynamic-extent *store-controller*))
       (unwind-protect 
	    (progn ,@body)
	 (setf *store-controller* *old-store*)))))

(defmacro with-miner-transaction (args &body body)
  "Ensure we have a transaction over the stuff we care about"
  `(with-miner-store (args)
     (let ((*old-state* *auto-commit*)
	   (*auto-commit* nil))
       (unwind-protect
	    (with-transaction ,(append (list :store-controller '*conceptminer-db*) args)
	      ,@body)
	 (setf *auto-commit* *old-state*)))))

;;
;; Open, initialize and shutdown Miner Object Store
;;

(defmacro ensure-miner-structure (var tag init-form)
  `(progn
     (unless (get-from-miner-root ,tag)
       (add-to-miner-root ,tag ,init-form))
     (setf ,var (get-from-miner-root ,tag))))

(defun open-miner-object-store ()
  (when *conceptminer-db*
    (warn "Miner is already open: ignoring open request")
    (return-from open-miner-object-store))
  (setf *conceptminer-db* (open-store *conceptminer-db-path* :recover t :deadlock-detect nil))
  ;; Restore langutils via DB
  (init-langutils-for-miner)
  ;; Inverse query index
  (ensure-miner-structure *conceptminer-inverse-query-index* 
			  :miner-inverse-query-index
			  (make-instance 'inverse-index :lemma nil))
  ;; Inverse page index
  (ensure-miner-structure *conceptminer-inverse-page-index* 
			  :miner-inverse-page-index
			  (make-instance 'inverse-index :lemma t))
  ;; query-to-page-map associations
  (ensure-miner-structure *conceptminer-query-to-page-map*
			  :query-to-page-map
			  (make-instance 'persistent-map))
  ;; instance-query-to-page-map associations
  (ensure-miner-structure *conceptminer-instance-query-to-page-map*
			  :instance-query-to-page-map
			  (make-instance 'persistent-map))
  ;; pattern to instance associations
  (ensure-miner-structure *conceptminer-instance-to-pattern-map*
			  :instance-to-pattern-map
			  (make-instance 'persistent-map))
  ;; pattern to instance page
  (ensure-miner-structure *conceptminer-instance-to-page-map*
			  :instance-to-page-map
			  (make-instance 'persistent-map))
  ;; Return the DB reference
  (unless (get-from-miner-root :cnet-graph)
    (add-to-miner-root :cnet-graph (make-instance 'cnet-graph)))
  (setf *cnet-graph* (get-from-miner-root :cnet-graph))
  *conceptminer-db*)

(defun close-miner-object-store ()
  (close-store *conceptminer-db*)
  (setf *cnet-graph* nil
	*conceptminer-db* nil
	*conceptminer-inverse-query-index* nil
	*conceptminer-inverse-page-index* nil
	*conceptminer-query-to-page-map* nil))

;;
;; Langutils Tokens
;;

(defvar *token-btree* nil)

(defun initialize-token-btree ()
  (setf *token-btree*
	(aif-ret (get-from-miner-root :token-btree)
	  (with-miner-transaction ()
	    (let ((ibtree (make-indexed-btree)))
	      (add-index ibtree 
			 :index-name :token 
			 :key-form '(lambda (idx key value)
				     (declare (ignore idx key))
				     (values t value))
			 :populate nil)
	      (add-to-miner-root :token-btree ibtree)
	      ibtree)))))

(defun miner-token-for-id (id)
  (get-value id *token-btree*))

(defun miner-id-for-token (token)
  (get-primary-key token (get-index *token-btree* :token)))

(defun miner-add-to-token-map (token id)
  (with-miner-transaction (:txn-nosync t)
    (setf (get-value id *token-btree*) token)))

(defvar *token-counter* nil)

(defun miner-token-id-counter ()
  (add-to-miner-root :token-counter (incf *token-counter*)))

(defun init-langutils-for-miner ()
  (setf langutils::*external-token-map* t)
  (initialize-token-btree)
  (aif (get-from-miner-root :token-counter)
       (setf *token-counter* it)
       (setf *token-counter* 0))
  (langutils::add-external-mapping #'miner-id-for-token #'miner-token-for-id 
				   #'miner-add-to-token-map #'miner-token-id-counter)
  (init-langutils))

(defun ensure-token-table-downcase ()
  (loop for id from 1 upto *token-counter* do
       (miner-add-to-token-map (miner-token-for-id id) id)))
