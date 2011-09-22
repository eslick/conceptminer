
(in-package :conceptminer)

;;
;; An aggregate cursor for finding pages by simple co-location
;;

(defclass and-cursor ()
  ((cursor-list :accessor and-cursor-list :initarg :cursor-list)))

(defmethod cursor-first ((cursor and-cursor))
  (mapc #'cursor-first (and-cursor-list cursor)))

(defmethod cursor-close ((cursor and-cursor))
  (mapc #'cursor-close (and-cursor-list cursor)))

(defmethod cursor-next ((cursor and-cursor))
  (with-slots (cursor-list) cursor
    (multiple-value-bind (valid? page set) (cursor-next (car cursor-list))
      (declare (ignore set))
      (if (not valid?)
	  (return-from cursor-next nil)
	  (case (and-cursor-search (cdr cursor-list) page)
	    (:found      (values t page nil))
	    (:not-found  (cursor-next cursor))
	    (:terminate  nil))))))

(defun and-cursor-search (list page)
  (if (null list)
      :found
      (multiple-value-bind (valid? newpage set) (cursor-set-range (car list) page)
	(declare (ignore set))
	(cond ((not valid?)
	       :terminate)
	      ((eq newpage page)
	       (and-cursor-search (cdr list) page))
	      (t 
	       (cursor-prev (car list))
	       :not-found)))))

;;
;; A wrapper around token-index cursors to handle location set searches
;;

(defstruct query-cursor db-cursor size range array offset)

(defmethod cursor-first ((cur query-cursor)) (cursor-first (query-cursor-db-cursor cur)))
(defmethod cursor-next ((cur query-cursor)) (cursor-next (query-cursor-db-cursor cur)))
(defmethod cursor-prev ((cur query-cursor)) (cursor-prev (query-cursor-db-cursor cur)))
(defmethod cursor-set-range ((cur query-cursor) key) (cursor-set-range (query-cursor-db-cursor cur) key))
(defmethod cursor-close ((cur query-cursor)) (cursor-close (query-cursor-db-cursor cur)))

(defun update-query-cursor-array (cursor)
  (multiple-value-bind (valid? page set) (cursor-current (query-cursor-db-cursor cursor))
    (declare (ignore page))
    (assert (and valid? set))
    (setf (query-cursor-array cursor) (location-set-array set))
    (setf (query-cursor-offset cursor) 0)))

