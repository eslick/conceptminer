(in-package :conceptminer)


;;
;; Simple, storable window object
;;

(defclass window ()
  ((query :accessor window-query :initarg :query)
   (page :accessor window-page :initarg :page)
   (start :accessor window-start :initarg :start)
   (length :accessor window-length :initarg :length)))

(defvar *window-phrases* (make-hash-table :weak-keys t))

(defmethod window-phrase ((win window))
  (aif-ret (gethash win *window-phrases*)
	   (setf (gethash win *window-phrases*)
		 (make-phrase-from-vdoc (page-contents (window-page win))
					(window-start win)
					(window-length win)))))

(defparameter *%windows%* nil)

(defun get-windows (instances)
  (setf *%windows%* nil)
  (let* ((association (make-string-association))
	 (miner (make-window-miner :read-from instances :write-to '*%windows%*
				   :searcher-class 'a9-searcher :association association)))
    (execute miner)
    (wait-for-miner miner)
    (release-pattern-miner miner)
    *%windows%*))
    
	 
    
  