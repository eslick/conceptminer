(in-package :conceptminer)

;; Instance mining with patterns

(defpclass extraction-pattern ()
  ((pattern :accessor extraction-pattern :initarg :pattern)
   (index-expr :accessor extraction-pattern-index-expr :initarg :index-expr :index t)))

(defun pattern-equal (e1 e2)
  (and (equal (pattern-before-expr e1) (pattern-before-expr e2))
       (equal (pattern-first-expr e1) (pattern-first-expr e2))
       (equal (pattern-middle-expr e1) (pattern-middle-expr e2))
       (equal (pattern-second-expr e1) (pattern-second-expr e2))
       (equal (pattern-after-expr e1) (pattern-after-expr e2))))

(defun get-extraction-pattern-by-index-expr (expr)
  (get-instances-by-value 'extraction-pattern 'index-expr expr))

(defun extraction-pattern-index-fn (pattern)
  (aif-ret (pattern-middle-expr pattern)
	   (aif (pattern-after-expr pattern)
		(first it)
		(aif (pattern-before-expr pattern)
		     (first it)
		     nil))))

(defun get-extraction-pattern (pattern)
  (if (eq (type-of pattern) 'extraction-pattern)
      pattern
      (let ((db-patterns (get-extraction-pattern-by-index-expr (extraction-pattern-index-fn pattern))))
	(let ((db-pattern (find pattern db-patterns :key #'extraction-pattern :test #'pattern-equal)))
	  (aif-ret db-pattern
		   (make-instance 'extraction-pattern
				  :pattern pattern
				  :index-expr (extraction-pattern-index-fn pattern)))))))

(defpclass instance-query ()
  ((pattern :accessor instance-query-extraction-pattern :initarg :pattern :index t)
   (phrase :accessor instance-query-phrase :initarg :phrase :initform nil)
   (location :accessor instance-query-location :initarg :location :type (member :first :second nil) :initform nil)))

(defun get-instance-query-for-pattern (extraction-pattern)
  (unless (eq (type-of extraction-pattern) 'extraction-pattern)
    (setf extraction-pattern (get-extraction-pattern extraction-pattern)))
  (get-instances-by-value 'instance-query 'pattern extraction-pattern))

(defmethod instance-query-pattern ((q instance-query))
  (extraction-pattern (instance-query-extraction-pattern q)))

(defmethod print-object ((obj instance-query) stream)
  (format stream "#<INST-QUERY (~A): ...~A...>"
	  (length-of (instance-query-pattern obj))
	  (pattern-signature
	   (if (instance-query-phrase obj)
	       (instantiate-pattern-expression 
		(instance-query-pattern obj)
		(instance-query-phrase obj)
		(ecase (instance-query-location obj)
		  (:first t)
		  (:second nil)))
	       (instance-query-pattern obj))
	   2)))

(defmethod print-discovered-relation ((query instance-query) pair)
  (format t "Extracted (~A \"~A\" \"~A\") ~A~%"
	  (string-upcase (instance-query-type query))
	  (token-array->string (phrase-pair-source pair))
	  (token-array->string (phrase-pair-target pair))
	  pair))

(defmethod extract-pattern-phrases ((query instance-query) (page page))
  "Return the relation pair, accommodating order differences"
  (when (page-contents page)
    (let ((pattern (instance-query-pattern query)))
      (loop for instance in (find-pattern-instances pattern (page-contents page))
	 collecting
	   (if (null (instance-query-phrase query))
	       (case (pattern-direction (instance-query-pattern query))
		 (:forward (list (get-pattern-instance-source pattern instance)
				 (get-pattern-instance-target pattern instance)))
		 (:backward (list (get-pattern-instance-target pattern instance)
				  (get-pattern-instance-source pattern instance))))
	       (case (instance-query-location query)
		 (:first (list (instance-query-phrase query)
			       (get-pattern-instance-target pattern instance)))
		 (:second (list (get-pattern-instance-source pattern instance)
				(instance-query-phrase query)))))))))

(defmethod instance-query-type ((query instance-query))
  (pattern-typename (instance-query-pattern query)))

(defmethod make-instance-query ((pattern pattern) &optional (phrase nil) (location :first))
  (aif-ret (loop for query in (get-instance-query-for-pattern (get-extraction-pattern pattern)) do
		(if (or (and (null phrase) (null (instance-query-phrase query)))
			(and (not (null phrase))
			     (not (null (instance-query-phrase query)))
			     (equalp (instance-query-phrase query) phrase)
			     (eq (instance-query-location query) location)))
		    (return query)))
	   (make-instance 'instance-query
			  :pattern (get-extraction-pattern pattern)
			  :phrase phrase
			  :location location)))


(defun generate-instance-query (instance-rec)
  (cond ((consp instance-rec)
;;	 (assert (and (eq (type-of (first instance-rec)) 'pattern)
;;		      (eq (type-of (second instance-rec)) 'phrase)))
	 (list (make-instance-query (first instance-rec) (second instance-rec) (third instance-rec))))
	((has-class 'pattern instance-rec) 
	 (list (make-instance-query instance-rec)))))

;; 
;; Extract instances
;; 

(defcomponent extract-instances
    (:vars (association *conceptminer-instance-query-to-page-map*))
    (:body
     (let* ((page data)
	    (query (car (get-inverse-associations page association))))
       (dolist (pair (extract-pattern-phrases query page))
	 (print-discovered-relation query pair)
	 (send (record-extracted-instance (instance-query-type query)
					  (instance-query-extraction-pattern query)
					  page
					  (first pair)
					  (second pair)))))))

(defun term->search-word (term)
  (cond ((null term)
	 (error "Should not have a null term"))
	((member term '(:first :second))
	 "*")
	((term-expr? term)
	 (token-for-id (term-word term)))
	((or (or-expr? term)
	     (and-expr? term))
	 "*")
	(t (error "Unknown term: ~A" term))))

(defun pattern-instance-words (query)
  (mapcar #'term->search-word
	  (if (instance-query-phrase query)
	      (pattern-expression (instantiate-pattern-expression
				   (instance-query-pattern query)
				   (instance-query-phrase query)
				   (ecase (instance-query-location query)
				     (:first t)
				     (:second nil))))
	      (pattern-expression (instance-query-pattern query)))))

(defun pattern-instance-search (searcher query)
  (let ((words (pattern-instance-words query)))
    (search-for-pattern-instance
     searcher 
     (apply #'concatenate 'string (shuffle words (repeat "+" (1- (length words))))))))

(defparameter *instance-list* nil)

(defcontainer instance-miner
  (:children 
   (igen list-generator :list *instance-list* :rate 100 :manual nil)
   (qgen send-fn-results :fn 'generate-instance-query :threshold 10)
   (search url-fn-search :threshold 10 :threaded t
	   :searcher-class 'google-searcher 
	   :fn 'pattern-instance-search
	   :association *conceptminer-instance-query-to-page-map*
	   :urls 25
	   )
   (url-filter filter :filter-fn 'page-filter)
   (fetch fetch-url :numprocs 10)
   (tagger text-tagger :quanta 20 :print t)
   (instance-extractor extract-instances)
   (relay handle-markers :fn nil :type-filter :end-of-results))
  (:netlist
   (igen -> qgen)
   (qgen -> search)
   (search -> url-filter)
   (url-filter -> fetch)
   (fetch -> tagger)
   (tagger -> instance-extractor)
   (instance-extractor -> relay)))

;;
;; Control 
;;

(defvar *instance-miner-instance* nil)

(defun make-instance-miner (instance-pattern-recs)
  "Takes a pattern for pair mining or a list of three
   elements (pattern phrase location)"
  (setf *instance-miner-instance*
	(make-container 'instance-miner
			:initrecs
			`((search :searcher-class google-searcher)
			  (igen :list ,instance-pattern-recs)))))

;;
;; API
;;

(defmethod mine-instance ((miner instance-miner) pattern instance location &aux results)
   (labels ((results-handler (instance)
 	     (push instance results)))
     (set-child-slot-value #'results-handler miner 'relay 'fn)
     (send-data miner 'qgen (if instance
				(list pattern instance location)
				pattern))
     (wait-for-miner miner)
     results))

(defun mine-for-concepts ( patterns concept-strings )
  (mapcar #'(lambda (string)
	      (mine-for-concept patterns (string->token-array string) :first))
	  concept-strings))

(defun mine-for-concept (patterns instance location)
  (batch-mine-instances 
   (make-pattern-instance-recs patterns instance location)))

(defun make-pattern-instance-recs (patterns instance location)
  (mapcar #'(lambda (pat) (list pat instance location)) patterns))

(defun batch-mine-instances (pattern-recs)
  "Given a list with each element being a pattern or (pattern, instance, location) list 
   find all instances of that on the web"
  (let ((miner (make-instance-miner pattern-recs)))
    (unwind-protect 
	   (progn (execute miner)
		  (wait-for-miner miner)
		  (mapcan #'get-instances-for-pattern 
			  (mapcar #'(lambda (rec) 
				      (get-extraction-pattern (if (listp rec)
								  (first rec)
								rec)))
				  pattern-recs)))
	(release-instance-miner miner))))

(defmethod miner-done-p ((miner instance-miner))
  (and (every #'queue-empty-p
	      (mapcar #'pcomp::input-queue
		      (pcomp::container-children miner)))
       (every #'worker-quiescent-p
	      (slot-value (get-child miner 'fetch ) 'pool))
       (eq :terminated (component-state (get-child miner 'igen)))))

(defun release-instance-miner (miner)
  (terminate miner)
  (sleep 2.0))
