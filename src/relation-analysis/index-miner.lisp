
(in-package :conceptminer)

;; ================================
;; Containers
;; ================================

(defcontainer cnet-single-relation-study
  (:children
   ;; Generate inputs
   (rgen list-generator :list effectof :rate 50 :manual nil)
   ;; Processing
   (qgen cnet-query-generator)
   (search url-search :searcher-class 'a9-searcher :urls 200 :threshold 5 
	   :threaded t)
   (url-filter filter :filter-fn 'page-filter)
   (fetch fetch-url :numprocs 20)
   (tagger text-tagger :quanta 10 :print t)
   (indexer page-indexer)
   (relation-accum list-accumulator :write-to '*relations-processed* :op 'pushnew)
   ;; Inspection
   (print-query query-string-printer)
   (print-url page-printer)
   (print-indexed page-printer :msg "indexed"))
  (:netlist
   (rgen -> qgen relation-accum)
   (qgen -> print-query search)
   (search -> url-filter)
   (url-filter -> print-url fetch)
   (fetch -> tagger)
   (tagger -> indexer)
   (indexer -> print-indexed)))

