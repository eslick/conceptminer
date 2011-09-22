;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: eventminer -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          logdb.lisp
;;;; Purpose:       Persistent log of processing exceptions
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  February 2006
;;;;

(in-package :conceptminer)

(defpclass log-entry ()
  ((component-name :accessor log-component-name :initarg :component-name :index t)
   (type :accessor log-type :initarg :type)
   (timestamp :accessor log-timestamp :initarg :timestamp :initform (get-universal-time) :index t)
   (input :accessor log-input :initarg :input)
   (condition :accessor log-condition :initarg :condition :initform nil)
   (message :accessor log-message :initarg :message :initform "")))

(defparameter *log-types* 
  '(:error :warning :note))

(defun make-log-entry (component-name type input &key condition (message ""))
  (with-miner-store ()
    (make-instance 'log-entry 
		   :component-name component-name
		   :type type
		   :input input
		   :condition condition
		   :message message)))

(defmacro write-plog (type message)
  `(make-log-entry (component-name self) ,type data :message message))

;;
;; Simple queries
;;

(defun find-all-logs ()
  (with-miner-store ()
    (get-instances-by-class 'log-entry)))

(defun find-component-logs (name)
  (with-miner-store ()
    (get-instances-by-value 'log-entry 'component-name name)))

(defun find-logs-by-time (starttime endtime)
  (with-miner-store ()
    (get-instances-by-range 'log-entry 'timestamp starttime endtime)))

(defun find-component-logs-by-time (name starttime endtime)
  (with-miner-store ()
    (let ((by-time (get-logs-by-time starttime endtime))
	  (by-component (get-component-logs name)))
      (flet ((filter (log) (member log by-time)))
	(select-if #'filter by-component)))))


(defun find-recent-logs (amount &key (unit :minutes) type component-name condition-type message)
  (macrolet ((testeq (var val-form)    `(if ,var 
					    (eq var ,val-form) t))
	     (testequal (var val-form) `(if ,var
					    (equal var ,val-form) t)))
    (labels ((filter (log)
	       (and (testeq type (log-type log))
		    (testeq component-name (log-component-name log))
		    (testeq condition-type (type-of (log-condition log)))
		    (testequal message (log-message log)))))
      (let* ((endtime (get-universal-time))
	     (startime (subtract-interval (get-universal-time) unit amount)))
	(with-miner-store ()
	  (select-if 
	   #'filter
	   (get-instances-by-range 'log-entry 'timestamp starttime (get-universal-time))))))))
		     



