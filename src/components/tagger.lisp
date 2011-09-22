;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: eventminer -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          tagger.lisp
;;;; Purpose:       Take a page reference & text content and process it
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  April 2006
;;;;

(in-package :conceptminer)

(defcomponent-ele text-tagger
  (:vars (print nil) (quanta 1) (forward-tagged-p nil))
  (:body
   (labels ((process-data (record)
	      (unless (pcomp-marker-p record)
	      (let ((page (car record))
		    (contents (cdr record)))
		(assert (subtypep (type-of page) 'page))
		(if (or (eq (page-status page) :tagged)
			(eq (type-of (page-contents page)) 'vector-document))
		    (send page)
		    (progn
		      (assert (stringp contents))
		      (when (< (length contents) 100000)
			(setf (page-contents page) (vector-tag contents))
			(setf (page-status page) :tagged)
			(when print
			  (format t "Tagged ~A~%" (page-url page))))
		      (send page)))))))
     (process-data data)
     (let ((count 0))
       (while (<= (incf count) quanta)
	 (let ((new-data (receive)))
	   (if new-data
	       (process-data new-data)
	     (progn 
	       (excl:gc)
	       (return)))))))))
