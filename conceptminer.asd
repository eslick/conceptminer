(in-package :cl-user)

(defpackage :conceptminer-build
  (:use :cl :asdf :asdf-config))

(in-package :conceptminer-build)

(defsystem-config :conceptminer
   
    :components (;; Basic miner infrastructure
		 (:module :src
   		     :components
		     ((:file "package")
	              (:module :data
			       :components ((:file "odb")
					    (:file "time-utils")
			                    (:file "vector-list")
					    (:file "association")
					    (:file "logdb")
					    (:file "querydb")
					    (:file "pagedb"))
		          :serial t)
	              (:module :index
			  :components ((:file "locationset")
				       (:file "inverseindex")
				       (:file "cursors")
				       (:file "queries"))
		          :serial t)
		      (:module :components
			  :components ((:file "components")
				       (:file "searcher")
				       (:file "yahoo")
				       (:file "a9")
				       (:file "google-scrape")
				       (:file "altavista")
				       (:file "search")
				       (:file "fetch")
				       (:file "extract")
				       (:file "tagger")
				       (:file "index")
				       )
			  :serial t)
		      (:module :classifiers
		           :components (
					(:file "classifiers")
					(:file "bayes-classifier")
					))
;; 		      (:module :omcs-analysis
;; 			  :components (
;; 				       (:file "label-omcs")
;; 				       (:file "omcs-eval")
;; 				       ))
		      (:module :relation-analysis
			  :components (
			               ;; Infrastructure
				       (:file "relations")
				       (:file "surface-forms")
				       (:file "relation-components")
				       (:file "index-miner") ;; live
				       (:file "windows-from-index") ;; cached
				       ;; Classifier based approach
				       (:file "near-miner") ;; live
				       (:file "windows-from-near-queries") ;; cached
				       (:file "window-preprocess") ;; filters
				       (:file "window-features") ;; generators
				       (:file "classifier-experiments") ;; classifier experiments
				       (:file "verify-by-classifier") ;; web query for pair
				       (:file "discoverer")  ;; use classifiers to find knowledge within a document
				       ;; Pattern-based approach
				       (:file "patterns") ;; extraction patterns
				       (:file "pattern-miner") ;; mutual bootstrapping from instances
				       (:file "instances")     ;; Records of proposed instance pairs
				       (:file "instance-miner") ;; Mine pairs, sources or targets from web
				       (:file "verify-by-patterns") ;; verify instances has R by patterns
				       (:file "instance-ranking")
				       (:file "concept-statistics")
				       (:file "demo")
		 		       )
			  :depends-on (:components)
			  :serial t)
		      )
		  :serial t))
    :serial t
    :depends-on (:langutils
                 :conceptnet
		 :cl-yahoo
		 :pcomp
		 :ele-bdb)
    :in-order-to ((load-op (compile-op :conceptminer)))
    :parameters ((:page-db-location "conceptminer::*conceptminer-db-path*"
				    :required t))
    :initialization "conceptminer::init-conceptminer")
