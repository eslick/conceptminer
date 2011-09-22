;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: eventminer -*-

(in-package :conceptminer)


;;
;; Search engine plugin API
;;

(defgeneric do-search (searcher query &rest rest)
  (:documentation "This generic function must be defined for any search objects
   that seek to support searching indexed documents using traditional web query
   strings"))

(defgeneric search-for-phrase (searcher query &rest rest)
  (:documentation "Search for a single contiguous set of words"))

(defgeneric search-for-proximate-phrases (searcher query1 query2 distance &rest rest)
  (:documentation "Search for two phrases separated by up to distance"))

;;
;; Useful base class
;;

(defclass document-searcher ()
  ((max-refs :accessor searcher-max-refs :initform 10 :initarg :max-refs)))

(defmethod quoted-phrases-p ((s document-searcher)) 
  t)

(defmethod search-for-phrase ((s document-searcher) phrase-as-string &key &allow-other-keys)
  (do-search s (concatenate "\"" phrase-as-string "\"")))

(defmethod search-for-proximate-phrases ((s document-searcher) phrase1 phrase2 distance &key &allow-other-keys)
  (declare (ignore phrase1 phrase2 distance))
  (error "Searcher class ~A does not support proximate phrase search" (class-of s)))

  

  
