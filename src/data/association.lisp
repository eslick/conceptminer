
(in-package :conceptminer)

;; Cheap associations instead of slot indices for finding relationships
;; between objects (need a cheap set mechanism - this is a btree that
;; maintains an index (so n log n size, log n lookup) to a set maintained
;; as an array (infrequent updates)

;;
;; Association BTREE
;;

(defclass persistent-map ()
  ((index :accessor persistent-map-index)
   (source-type :accessor persistent-map-source-type :initarg :source-type :initform nil)
   (target-type :accessor persistent-map-target-type :initarg :target-type :initform nil)))

(defmethod initialize-instance :after ((o persistent-map) &rest initargs)
  "If you want a custom store, wrap it with with-store-controller"
  (declare (ignore initargs))
  (setf (persistent-map-index o) (make-btree)))

(defmethod add-association ((src persistent) (targ persistent) (pm persistent-map))
  "Enter a single association between two persistent objects.  This abstraction works
   most efficiently when a domain is mapped to small subsets of a range but is not
   1:1.  For example, a query can associated with dozens of results without putting
   the query as an indexed slot of the result.  Because we store using only oids
   we need to have a single type of target!"
  (unless (persistent-map-source-type pm)
    (setf (persistent-map-source-type pm) (type-of src)))
  (assert (has-class (persistent-map-source-type pm) src))

  (unless (persistent-map-target-type pm)
    (setf (persistent-map-target-type pm) (type-of targ)))
  (assert (has-class (persistent-map-target-type pm) targ))

  (with-transaction (:store-controller (elephant::get-con (persistent-map-index pm)))
    (let ((vlist (get-value (elephant::oid src) (persistent-map-index pm))))
      ;; WARNING: Is this a good idea?  Why is the map corrupted?
      (unless (typep vlist 'vlist)
	(setf vlist nil))
      (setf (get-value (elephant::oid src) (persistent-map-index pm))
	    (if (null vlist)
		(add (elephant::oid targ) (make-vector-list :initial-size 3))
		(add (elephant::oid targ) vlist)))))
  t)

(defmethod get-associations ((src persistent) (pm persistent-map))
  "Enforce the oid only for storage & retrieval efficiency"
  (awhen (get-associations-as-oids src pm)
    (if (numberp it)
	(setf (get-value (elephant::oid src) (persistent-map-index pm)) nil)
	(loop for oid across (vlist-array it) collect 
	     (oid->object oid pm :target)))))

(defmethod get-associations-as-oids ((src persistent) (pm persistent-map))
  "Enforce the oid only for storage & retrieval efficiency"
  (get-value (elephant::oid src) (persistent-map-index pm)))
  
(defmethod get-inverse-association-as-oids ((targ persistent) (pm persistent-map) &aux keys)
  "Find all associations for targ.  O(N) operation!  Returns a list of oids"
  (map-btree (lambda (key val)
	       (when (typep val 'vlist)
		 (loop for oid across (vlist-array val) do
		      (when (eq (elephant::oid targ) oid)
			(push key keys)))))
	     (persistent-map-index pm))
  keys)

(defmethod get-inverse-associations ((targ persistent) (pm persistent-map))
  (loop for oid in (get-inverse-association-as-oids targ pm) collect 
       (oid->object oid pm :key)))

(defmethod drop-association ((src persistent) (targ persistent) (pm persistent-map))
  (setf (get-value (elephant::oid src) (persistent-map-index pm))
	(drop (get-value (elephant::oid src) (persistent-map-index pm))
	      (elephant::oid targ))))

(defmethod drop ((pm persistent-map) (src persistent))
  (remove-kv (elephant::oid src) (persistent-map-index pm)))

(defmethod migrate ((dst store-controller) (src persistent-map))
  "We need to support migration on associations since we break the
   normal persistent object abstraction"
  (let ((newmap (make-instance 'persistent-map)))
    (setf (persistent-map-source-type newmap) 
	  (persistent-map-source-type src))
    (setf (persistent-map-target-type newmap) 
	  (persistent-map-target-type src))
    (if (elephant::object-was-copied-p (persistent-map-index src))
	(setf (persistent-map-index newmap) (persistent-map-index src))
	(progn
	  (map-btree (lambda (koid vlist)
		       (let ((keyobj (migrate dst (oid->object koid src :key))))
			 (loop for toid across (vlist-array vlist) do
			      (add-association keyobj 
					       (migrate dst (oid->object toid src :target))
					       newmap))))
		       (persistent-map-index src))
	  (elephant::register-copied-object (persistent-map-index src)
					    (persistent-map-index newmap))))
    newmap))

(defmethod oid->object (oid (pm persistent-map) &optional (type :key))
  "Converts oid based on type of :key or :value"
  (elephant::get-cached-instance (elephant::get-con (persistent-map-index pm)) 
				 oid
				 (case type
				   (:key (persistent-map-source-type pm))
				   (:source (persistent-map-source-type pm))
				   (:value (persistent-map-target-type pm))
				   (:target (persistent-map-target-type pm)))))
			 

;;
;; Double association btree (TODO)
;;

;; (defpclass double-association-btree ()
;;   ((fwd :accessor dassoc-btree-forward :initarg :fwd)
;;    (bck :accessor dassoc-btree-backward :initarg :bck)))

;; (defmethod add-association ((src persistent) (targ persistent) (dab double-association-btree))
;;   (add-association src targ (dassoc-btree-forward dab))
;;   (add-association targ src (dassoc-btree-backward dab)))

;; (defmethod get-value ((src persistent) (dab double-association-btree))
;;   (aif-ret (get-value src (dassoc-btree-forward dab))
;; 	   (get-value src (dassoc-btree-backward dab))))

;; (defmethod (setf get-value) ((dst persistent) (src persistent) (dab double-association-btree))
;;   (add-association src dst dab))


