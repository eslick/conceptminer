;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: eventminer -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          index.lisp
;;;; Purpose:       Index tagged page content
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  April 2006
;;;;

(in-package :conceptminer)

(defcomponent page-indexer
  (:body 
   (labels ((process-data (data)
	      (assert (subtypep (type-of data) 'page))
	      (assert (subtypep (type-of (page-contents data)) 'vector-document))
;;	      (format t "contents: ~A~%" (page-contents data))
	      (index-page data)
	      (send data)))
;;     (format t "received page ~A w/ text type: ~A~%" 
;;	     data 
;;	     (or (null (page-contents data)) (type-of (document-text (page-contents data)))))
     (unless (or (and (slot-boundp data 'page-indexed) (page-indexed-p data))
		 (null (page-contents data))
		 (subtypep (type-of (document-text (page-contents data)))
			   'vector-document))
       (process-data data)))))
