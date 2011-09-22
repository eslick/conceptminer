;; A simple association that's not persistent

(in-package :conceptminer)

(defclass simple-association ()
  ((table :accessor simple-association-table :initarg :table :initform (make-hash-table)))
  (:documentation
   "This is a fairly slow, list-based way to keep forward associations between a class of
    source objects and a class of destination objects"))

(defmethod add-association (src targ (assoc simple-association)) 
  (pushnew targ (gethash src (simple-association-table assoc))
	   :test (hash-table-test (simple-association-table assoc))))

(defmethod get-associations (src (assoc simple-association))
  (gethash src (simple-association-table assoc)))

(defmethod get-inverse-associations (targ (assoc simple-association) &aux results)
  (do-hash (src targs (simple-association-table assoc))
    (dolist (cur-targ targs)
      (when (funcall (hash-table-test (simple-association-table assoc)) targ cur-targ)
	(push src results)
	(return))))
  results)

(defun make-association (&key (test #'eq))
  (make-instance 'simple-association 
		 :table (make-hash-table :test test)))

(defun make-string-association ()
  (make-association :test #'equal))
