
(in-package :conceptminer)

(defparameter *vector-list-default-length* 4)

(defclass vlist ()
  ((array :accessor vlist-array :initarg :array)
   (unique-p :accessor vlist-unique-p :initarg :unique-p)
   (type :accessor vlist-type :initarg :type)))

(defun make-vector-list (&key 
			 (initial-size *vector-list-default-length*)
			 (initial-element 0)
			 (type 'fixnum)
			 (unique-p nil))
  (let ((vlist
	 (make-instance 'vlist
			:array (make-array initial-size 
					   :initial-element initial-element
					   :adjustable t
					   :fill-pointer t
					   :element-type type)
			:unique-p unique-p
			:type type)))
    (setf (fill-pointer (vlist-array vlist)) 0)
    vlist))

(defmethod add (elt (vl vlist))
  "Add an element to the vlist"
  (assert (or (null (vlist-type vl))
	      (eq (type-of elt) (vlist-type vl))))
  (when (= (fill-pointer (vlist-array vl)) (array-total-size (vlist-array vl)))
    (setf (vlist-array vl)
	  (adjust-array (vlist-array vl) 
			(+ (length (vlist-array vl)) 4)
			:initial-element 0)))
  (if (vlist-unique-p vl)
      (progn
	(loop for i from 0 upto (length (vlist-array vl))
	   for entry across (vlist-array vl) do
	     (when (equal entry elt)
	       (return-from add vl)))
	(vector-push elt (vlist-array vl)))
      (vector-push elt (vlist-array vl)))
  vl)

(defmethod drop ((vl vlist) elt)
  "Remove an element eq to elt from the vlist"
  (loop 
     for i from (length (vlist-array vl)) downto 0 do
     (when (equal (aref (vlist-array vl) i) elt)
       (setf (vlist-array vl)
	     (vector-1d-lshift (vlist-array vl) i 1))
       (return-from drop elt)))
  vl)

(defmethod find-value ((vl vlist) val)
  (loop for i from (length (vlist-array vl)) downto 0 do
       (when (equal (aref (vlist-array vl) i) val)
	 (return-from find-value t))
       finally (progn nil)))

(defmethod length-of ((vl vlist))
  (length (vlist-array vl)))

(defmethod print-object ((vl vlist) stream)
  (format stream "#VLIST(~{~A ~})" 
	  (array->list (subseq (vlist-array vl) 0 (min (length-of vl) 5)))))

(defmethod vlist->list ((vl vlist))
  (array->list (vlist-array vl)))

;; ====================================
;; VSET - restricted version of vlist
;; ====================================

(defclass vset (vlist) 
  ())

(defmethod initialize-instance :after ((vset vset) &rest rest)
  (declare (ignore rest))
  (setf (vlist-unique-p vset) t))

