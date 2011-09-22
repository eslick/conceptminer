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

;;
;; Records of page data
;;

(defpclass page ()
  ((url :accessor page-url :initarg :url)
   (timestamp :accessor page-timestamp :initarg :timestamp :initform (get-universal-time))
   (type :accessor document-type :initarg :type :initform :web-html)
   (contents :accessor page-contents :initarg :contents :initform nil)
   (size :accessor page-size :initarg :size)
   (status :accessor page-status :initarg :status :initform) ;; :fetched :tagged :indexed
   (page-indexed :accessor page-indexed-p :initarg :page-indexed)
   (info :accessor page-info :initarg :info))
  (:index t))

(defmethod page-indexed-p :around ((p page))
  (if (slot-boundp p 'page-indexed)
      (call-next-method)
      nil))

(defmethod initialize-instance :before ((p page) &rest rest &key url)
  (ensure-url-index)
  (when (and url (lookup-pages-by-url url))
   (error "Check for duplicate URLs before creating new pages")))

(defun make-page (url &key (contents nil))
  (with-miner-transaction ()
    (make-instance 'page 
		   :url url
		   :contents contents
		   :size (typecase contents
			   (null 0)
			   (string (length contents))
			   (vector-document (length-of contents))))))

(defmethod print-object ((obj page) stream)
  (format stream "#<PAGE ~A>" (page-url obj)))

(defmethod print-page-contents ((p page))
  (let ((contents (page-contents p)))
    (format t "~A"
	    (etypecase contents
	      (string contents)
	      (array (token-array->string contents))
	      (vector-document (print-vector-document contents :stream nil))))))

(defmethod (setf page-contents) :before (value (page page))
  "Safety measure so we don't end up with indexed pages in the inverse index
   wasting space that we can't remove without a full index search!"
  (declare (ignore value))
  (when (and (slot-boundp page 'page-indexed)
	     (page-indexed-p page))
    (error "Cannot change the contents of an indexed page.  Remove from
          index prior to changing contents.")))

;; 
;; Page indexing, queries and extra accessors
;; 

(defmethod page-host ((p page))
  (puri:URI-HOST (puri:PARSE-URI (page-url p))))

(defmethod page-content-type ((page page))
  (typecase (page-contents page)
    (string :stripped-html)
    (vector-document :tagged-text)
    (null :empty)
    (t (error "Invalid page content type"))))

;; Hashed inverse index of urls & query

(defmethod ensure-url-index ()
  (let ((hidx (find-inverted-index 'page 'url-hash :null-on-fail t)))
    (unless hidx
      (add-class-derived-index 'page 'url-hash 'url-index-fn))))

(defun url-index-fn (page)
  (string-hash-x31 (page-url page)))

(defmethod lookup-pages-by-url ((url string))
  (ensure-url-index)
  (collect (lambda (page)
	     (when (equal url (page-url page))
	       page))
	   (get-instances-by-hashed-value 'page 'url-hash url)))

(defun get-instances-by-hashed-value (class slot value)
  (get-instances-by-value class slot (string-hash-x31 value)))

;;
;; Utilities
;;

(defun token-array->string (array)
  (when (and (arrayp array)
	     (> (length array) 0))
    (apply #'concatenate 'string 
	   (shuffle
	    (map-across #'token-for-id array)
	    (repeat " " (1- (length array)))))))

(defun string-hash-x31 (string &aux (h 0))
  (declare (optimize (speed 3) (space 0) (safety 1))
	   (type fixnum h))
  (assert (simple-string-p string))
  (loop for ch across string do
       (setf h (mask-field (byte 29 0) (+ (- (ash h 5) h) (char-code ch)))))
  h)
        

;; X31_HASH from glibc
;;guint hash (gconstpointer v)
;;{
;;  const char *p;
;;  guint h=0;
;;  for(p = (const char*) v; *p != '\0'; p += 1) {
;;    h = ( h << 5 ) - h + *p;
;;  }
;;  return h;
;;}






