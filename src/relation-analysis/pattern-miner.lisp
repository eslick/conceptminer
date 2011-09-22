(in-package :conceptminer)

;;
;; Discover lexico-syntactic relation patterns and instance pairs
;;

;; forward reference
(defclass pattern-miner () ())

(defparameter *pattern-threshold-score* 0.7)
(defparameter *pattern-growth-threshold* 0.1)

(defun top-relation-patterns (relation-ids)
;;  (rank-relation-patterns
;;   (validate-relation-patterns 
;;    (generalize-relation-patterns
     (batch-mine-patterns relation-ids))

(defun batch-mine-patterns (relation-ids &optional force)
  (aif-ret (and (not force) (get-patterns-for-relation-ids relation-ids))
    (let ((miner (make-pattern-miner (make-relation-recs relation-ids))))
      (unwind-protect 
	   (progn (execute miner)
		  (wait-for-miner miner)
		  (get-patterns-for-relation-ids relation-ids))
	(release-pattern-miner miner)))))

(defmethod mine-pattern ((miner pattern-miner) relation-id &aux results)
  "Mine one relation at a time"
  (labels ((results-handler (relid)
	     (setf results (get-patterns-for-relation-id relid))))
    (set-child-slot-value #'results-handler miner 'relay 'fn)
    (send-data miner 'qgen (cons relation-id (get-relation relation-id)))
    (wait-for-miner miner)
    results))

;;
;; Pattern miner pcomp container
;;       

(defcomponent extract-patterns
  (:vars (association *conceptminer-query-to-page-map*))
  (:body
   (let* ((page data)
	  (query (car (get-inverse-associations page association))))
     (let ((patterns (extract-pattern-sentences query page *interior-pattern-instance-width* )))
       (format t "Extracted ~A patterns~%" (length patterns))
       (dolist (pattern patterns)
	 (send pattern))))))

(defcomponent handle-markers
  (:vars fn type-filter)
  (:marker 
   (assert (eq (type-of data) 'pcomp-data-marker))
   (format t "Received marker for: ~A~%" (marker-data data))
   (when (and fn (or (not type-filter)
		     (eq (marker-type data) type-filter)))
     (format t "Calling ~A for marker" fn)
     (funcall fn (marker-data data)))))


(defun relation-pattern-search (searcher query)
  (search-for-pattern-phrases
   searcher
   (token-array->string (query-source-phrase query))
   (token-array->string (query-target-phrase query))))

(defun generate-pattern-queries (relation-rec)
  (remove-nulls
   (mapcar (lambda (pair)
	     (make-cnet-query (car relation-rec) pair :pattern))
	   (let ((surface-pairs (relation-surface-pairs (cdr relation-rec))))
	     (append surface-pairs (mapcar #'reverse surface-pairs))))))

(defcontainer pattern-miner
  (:children
   (rgen list-generator :list *relation-list* :rate 100 :manual nil)
   (qgen send-fn-results :fn 'generate-pattern-queries :threshold 10)
   (search url-fn-search 
	   :urls 20
	   :searcher-class 'google-searcher
	   :fn 'relation-pattern-search 
	   :association *conceptminer-query-to-page-map*
	   :threshold 10 :threaded t)
   (url-filter filter :filter-fn 'page-filter)
   (fetch fetch-url :numprocs 10)
   (tagger text-tagger :quanta 20 :print t)
;;   (pattern-extractor extract-patterns)
   (relay handle-markers :fn nil :type-filter :end-of-results))
  (:netlist
   (rgen -> qgen)
   (qgen -> search)
   (search -> url-filter)
   (url-filter -> fetch)
   (fetch -> tagger)
;;   (tagger -> pattern-extractor)
;;   (pattern-extractor -> relay)))
   (tagger -> relay)))

(defvar *pattern-miner-instance* nil)

(defun make-pattern-miner (relation-recs)
  (setf *pattern-miner-instance*
	(make-container 'pattern-miner
			:initrecs
			`((search :searcher-class google-searcher)
			  (rgen :list ,relation-recs)))))

(defmethod miner-done-p ((miner pattern-miner))
  (and (every #'queue-empty-p
	      (mapcar #'pcomp::input-queue
		      (pcomp::container-children miner)))
       (every #'worker-quiescent-p
	      (slot-value (get-child miner 'fetch ) 'pool))
       (eq :terminated (component-state (get-child miner 'rgen)))))

(defun release-pattern-miner (miner)
  (terminate miner)
  (sleep 2.0))
;;  (kill-procs-by-substring "worker")
;;  (kill-procs-by-substring "SEARCH")



;;
;; Utilities
;;

(defun source-instance (instance-pair)
  (car instance-pair))
(defun target-instance (instance-pair)
  (cdr instance-pair))
(defun make-instance-pair (source target)
  (cons source target))

(defparameter *prospective-patterns* nil)

(defun clear-prospects ()
  (setf *prospective-patterns* nil))

(defun manual-select-if (elts &optional (print-fn #'print) &aux results)
  "Prints and then prompts for each element"
  (loop for elt in elts do
       (funcall print-fn elt)
       (let ((response (read-char)))
	 (read-char)
	 (print response)
	 (cond ((eq response #\y)
		(push elt results))
	       ((eq response #\q)
		(return-from manual-select-if results)))))
  results)
	       
		   

