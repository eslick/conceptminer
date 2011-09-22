;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: eventminer -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          relation-study-fast.lisp
;;;; Purpose:       Use altavista near operator to do fast queries
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  April 2006
;;;;

(in-package :conceptminer)

(defmethod proximate-cnet-search ((searcher altavista-searcher) query)
  (let ((relid (cnet-query-relation-id query)))
    (search-for-proximate-phrases
     searcher 
     (token-array->string (relation-source-phrase relid))
     (token-array->string (relation-target-phrase relid))
     nil)))

;;
;; Relation Training Window Generator
;;

(defcomponent extract-relation-training-windows 
  (:vars (width 30) (margin 10) (association *conceptminer-query-to-page-map*))
  (:body
   (let* ((page data)
	  (query (car (get-inverse-associations page association))))
     (dolist (window (extract-training-windows query page width margin))
       (send window)))))

;;
;; Mine relation windows using the NEAR query
;;

(defparameter *relation-list* nil)
(defparameter *window-list* nil)

(defcontainer cnet-relation-window-miner
  (:children 
   ;; Generate inputs
   (rgen list-generator :list *relation-list* :rate 100 :manual nil)
   ;; Search
   (qgen cnet-query-generator :threshold 10)
   (search url-fn-search :searcher-class 'altavista-searcher :urls 50
	   :fn 'proximate-cnet-search
	   :threshold 10 :threaded t)
   (url-filter filter :filter-fn 'page-filter)
   ;; Fetch
   (fetch fetch-url :numprocs 15)
   ;; Processing
   (tagger text-tagger :quanta 20 :print t)
   (window-extractor extract-relation-training-windows :size 30 :margin 10)
;;   (window-filter filter-windows)
   (accum list-accumulator :write-to '*window-list* :print t))
  (:netlist
   (rgen -> qgen)
   (qgen -> search)
   (search -> url-filter)
   (url-filter -> fetch)
   (fetch -> tagger)
   (tagger -> window-extractor)
   (window-extractor -> accum)))
;;   (window-extractor -> window-filter)
;;   (window-filter -> accum)))

      

      


			      