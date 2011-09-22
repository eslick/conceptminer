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

;;      
;; Locations within a document (for proximity comparison)
;;

(defclass location-set ()
  ((array :accessor location-set-array :initarg :array 
	  :initform (make-array 4 :element-type 'fixnum :fill-pointer 0 :adjustable t))
   (factor :accessor location-set-growth-factor :initform 0.7 :initarg :growth-factor))
  (:documentation "A low to high set of locations"))

(defmethod initialize-instance :around ((obj location-set) &rest initargs &key initial-contents)
  (cond ((null initial-contents)
	 (call-next-method))
	((consp initial-contents)
	 (apply #'call-next-method obj :array 
		(make-array (length initial-contents) 
			    :initial-contents (sort (copy-list initial-contents) #'<)
			    :fill-pointer t :adjustable t)
		(remove-keyword :initial-contents initargs)))
	((arrayp initial-contents)
	 (apply #'call-next-method obj :array initial-contents
		(remove-keyword :initial-contents initargs)))
	(t (error "Badly formed :initial-contents, must be a list (for now)"))))

(defmethod expansion-length ((ls location-set))
  (declare (optimize (speed 3) (safety 1) (space 0) (debug 0)))
  (let ((array (location-set-array ls))
	(factor (location-set-growth-factor ls)))
    (declare (type fixnum factor)
	     (type vector array))
    (ceiling (* (length array) factor))))
		    

(defmethod insert-location ((set location-set) location)
  (declare (type fixnum location)
	   (optimize (speed 3) (safety 1) (space 0) (debug 0)))
  (let* ((array (location-set-array set))
	 (length (length array))
	 (insert (loop 
		    for loc across array
		    for offset from 0 do
		    (cond ((= location loc)
			   (return-from insert-location set))
			  ((> loc location)
			   (return offset)))
                    finally 
		      (return (1+ offset)))))
    (declare (type fixnum length insert))
;;    (format t "insert ~A at ~A into ~A~%" location insert array)
    (when insert
      (unless (array-in-bounds-p array length)
	(setf array (adjust-array array (+ length (expansion-length set)))))
      (incf (fill-pointer array))
      (vector-1d-rshift array insert 1 :adjust nil)
      (setf (aref array insert) location)
      (setf (location-set-array set) array))
    set))

(defmethod delete-location ((set location-set) location)
  (declare (type fixnum location)
	   (optimize (speed 3) (safety 1) (space 0) (debug 0)))
  (let* ((array (location-set-array set))
	 (delete (position location array)))
    (declare (type fixnum length delete))
    (when delete
      (setf array (vector-1d-lshift array delete 1 :adjust nil))
      (decf (fill-pointer array))
      (setf (location-set-array set) array))
    set))

(defmethod find-windows ((ls1 location-set) (ls2 location-set) distance &key (ordered t) (duplicates nil))
  "Returns pairs of locations that are within 'distance' of each other.
   Setting ordered to true returns only pairs in ls2 that follow pairs
   in ls1"
  (declare (optimize (speed 3) (safety 1) (space 0) (debug 0)))
  (let ((result (make-array 2 :fill-pointer 0 :adjustable t))
	(array1 (location-set-array ls1))
	(array2 (location-set-array ls2)))
    (let ((o1 0) 
	  (o2 0))
      (loop while (and (< o1 (length array1)) 
		       (< o2 (length array2))) do
	   (let ((loc1 (aref array1 o1))
		 (loc2 (aref array2 o2)))
	     (let ((dir (< loc1 loc2))
		   (dist (abs (- loc2 loc1))))
;;	       (format t "result: ~A o1-~A:~A o2-~A:~A dir: ~A dist: ~A~%" 
;;		       result o1 loc1 o2 loc2 dir dist)
	       (cond ((and duplicates (= loc1 loc2))
		      (vector-push-extend loc1 result (expansion-length ls1))
;;		      (push-location result loc1)
		      (incf o1)
		      (incf o2))
		     ((and dir (<= dist distance))
		      (vector-push-extend loc2 result (expansion-length ls1))
;;		      (push-location result loc2)
		      (incf o1))
		     ((and (not dir) (<= dist distance))
		      (when (not ordered)
			(vector-push-extend loc1 result (expansion-length ls1)))
;;			(push-location result loc1))
		      (incf o2))
		     (t (if dir (incf o1) (incf o2))))))))
	(make-instance 'location-set 
	  :initial-contents result 
	  :growth-factor (location-set-growth-factor ls1))))

(defmethod find-adjacent-pairs ((ls1 location-set) (ls2 location-set))
  (find-windows ls1 ls2 1 :ordered t))

(defun find-adjacent-sequences (location-sets)
  "Takes an ordered set of location sets and finds all
   sequences where set1 contains words just before a
   word in set2, etc and returns the set of last 
   locations in the sequence"
  (accumulate-list #'find-adjacent-pairs location-sets))
			 
;; ======================
;; Tests 
;; ======================

(defun test-pairs ()
  (with-open-store ("/Users/eslick/Work/db/scratch/")
    (with-transaction ()
      (let ((ls1 (make-instance 'location-set :initial-contents '(1 5 10 15 20 26)))
	    (ls2 (make-instance 'location-set :initial-contents '(3 6 13 16 25 26)))
	    (ls3 (make-instance 'location-set :initial-contents '(4 8 17 26))))
	(format t "1: ~A~%" (location-set-array ls1))
	(format t "2: ~A~%" (location-set-array ls2))
	(format t "3: ~A~%" (location-set-array ls3))
	(format t "adjacent pairs 1,2: ~A~%" (location-set-array (find-adjacent-pairs ls1 ls2)))
	(format t "window dist 2 1,2: ~A~%" (location-set-array (find-windows ls1 ls2 2)))
	(format t "window dist +/-2 1,2: ~A~%" (location-set-array (find-windows ls1 ls2 2 :ordered nil)))
	(format t "window dist 3 1,2: ~A~%" (location-set-array (find-windows ls1 ls2 3)))
	(format t "sequence 1-3: ~A~%" (location-set-array (find-adjacent-sequences (list ls1 ls2 ls3)))))
      )))