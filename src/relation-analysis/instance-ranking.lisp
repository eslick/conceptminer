
(in-package :conceptminer)


;; ===================================
;; Lexical and Concept Filters
;; ===================================

(defun filter-instances-by-concepts (instances)
  (filter-if (lambda (inst)
	       (or (filter-new-concept (phrase-pair-source (phrase-pair inst)))
		   (filter-new-concept (phrase-pair-target (phrase-pair inst)))
		   (filter-concept-string (phrase-pair-source (phrase-pair inst)))
		   (filter-concept-string (phrase-pair-target (phrase-pair inst)))))
	     instances))

(defun filter-concept-string (phrase)
  "Filter a phrase by its funny characters, non-concept character
   length or other."
  (let ((str (as-string phrase))
        (arry (as-array  phrase)))
    (or (< (length str) 3)
	(> (length str) 40)
	(> (char-code (aref str 0)) 128)
	(> (char-code (aref str 1)) 128)
	(and (every #'stopword? arry)
	     (not (person-pronoun-p (aref arry 0))))
	(loop for char across str do
              (let ((code (char-code char)))
		(when (and (>= code (char-code #\0))
			   (<= code (char-code #\9)))
 		  (return t)))))))

(defun filter-new-concept (phrase)
  "Filter a full concept by whether it is in conceptnet, or if
   not is close enough to qualify as a new piece of knowledge"
  (let ((tokens (as-array phrase)))
    (if (find-pconcept (as-array phrase))
	nil
	(cond ((>= (length tokens) 2)
	       (let ((head-term (aref tokens 0))
		     (tail-term (aref tokens (1- (length tokens)))))
		 (not (or (find-pconcept (token-for-id tail-term))
			  (when (verb-pos? (get-lexicon-default-pos head-term))
			    (find-pconcept (token-for-id head-term)))))))
	      (t t)))))

;; ===================================
;; Path distance sort
;; ===================================

(defun filter-instances-by-path-distance (examples threshold)
  (select-if (lambda (ex) (path-score-< (get-path-score ex) threshold)) examples))

(defun sort-instances-by-path-distance (instances)
  (sort
   (mapcar #'(lambda (instance)
	       (list (get-path-score instance) instance))
	   instances)
   #'path-score-<
   :key #'first))

(defun path-score-< (ap bp)
  (cond ((and (= (first ap) 0) (not (= (first bp) 0)))
	 nil)
	((and (not (= (first ap) 0)) (= (first bp) 0))
	 t)
	((= (first ap) (first bp))
	 (> (second ap) (second bp)))
	(t 
	 (< (first ap) (first bp)))))

(defun get-path-score (instance)
  (aif-ret (and (slot-boundp instance 'path-length) (cnet-path-distance instance))
	   (let ((paths (conceptnet::find-shortest-paths
			 (token-array->string (phrase-pair-source (phrase-pair instance)))
			 (token-array->string (phrase-pair-target (phrase-pair instance))))))
	     (setf (cnet-path-distance instance)
		   (if (> (length paths) 0)
		       (list (length (first paths)) (length paths))
		       (list 0 0))))))

(defun get-path-median (instances)
  (let ((scores (filter-if (lambda (x) (equal '(0 0) x)) 
			   (sort (mapcar #'get-path-score instances)
				 #'path-score-<))))
    (nth (ceiling (/ (length scores) 2)) scores)))

;; Need to get good results
;; Measure instances against pattern set?

;; ========================================
;; PMI 
;; ========================================

(defun band-pass-filter-instances-by-pmi (instances ceiling floor)
  (let ((searcher (make-instance 'google-searcher)))
    (filter-if #'(lambda (inst)
		   (let ((pmi (simple-instance-pmi inst searcher)))
		     (or (> pmi ceiling)
			 (< pmi floor))))
	       instances)))

(defun sort-instances-by-pmi (instances)
  (let ((searcher (make-instance 'google-searcher)))
    (sort (mapcar #'(lambda (inst)
		      (list (simple-instance-pmi inst searcher) inst))
		  instances)
	  #'>
	  :key #'first)))

(defvar *pmi-cache* (make-hash-table :test #'equal))

(defun simple-instance-pmi (instance searcher)
  (with-transaction ()
    (let ((joint-string 
	   (format nil "\"~A\"+\"~A\"" 
		   (token-array->string (phrase-pair-source (phrase-pair instance)))
		   (token-array->string (phrase-pair-target (phrase-pair instance)))))
	  (source-string
	   (format nil "~A"
		   (token-array->string (phrase-pair-source (phrase-pair instance)))))
	  (target-string
	   (format nil "~A"
		   (token-array->string (phrase-pair-target (phrase-pair instance)))))
	  (pmi (mined-instance-pmi instance)))
      (if pmi
	  pmi
	  (setf (mined-instance-pmi instance)
		(compute-web-pmi
		 (get-cached-hit-count searcher joint-string)
		 (get-cached-hit-count searcher source-string)
		 (get-cached-hit-count searcher target-string)))))))

(defun get-cached-hit-count (searcher string)
  (aif-ret (gethash string *pmi-cache*)
	   (setf (gethash string *pmi-cache*)
		 (get-hit-count searcher string))))

(defparameter *google-max-hits* (coerce 25000000000 'float))

(defun compute-web-pmi (joint source target)
  (if (or (= 0 joint)
	  (= 0 source)
	  (= 0 target))
      0.0
      (coerce (/ joint source) 'float)))

;;      (coerce (log (/ (/ joint *google-max-hits*)
;;		      (* (1+ (/ source *google-max-hits*))
;;			 (1+ (/ target *google-max-hits*)))))
;;	      'float)))

(defun clear-pmi-calculations (instances)
  (map nil (lambda (x) (setf (mined-instance-pmi x) nil)) instances))

;; =======================================================
;; Sort by 'extraction' hits
;; =======================================================

;;(defun filter-instances-by-scores (instances threshold)
;;  (declare (ignore type))
;;  (select-if #'(lambda (x) (> (

(defun sort-instances-by-extraction-score (instances)
  (sort (copy-list instances) #'> :key #'get-pattern-extraction-score))

;; TODO: accumulate # of times a given pattern hits and augment this
;; score statistic

(defun get-pattern-extraction-score (instance)
  (aif-ret (extraction-pattern-score instance)
	   (setf (extraction-pattern-score instance)
		 (length (aif-ret (get-patterns-for-instance instance) 0)))))
				  
;; =======================================
;; Analysis of categories
;; =======================================

(defun manually-assign-category-labels (examples &key (skip t) &aux prior (count 0))
  (format t "Labelling ~A instances...~%" (length examples))
  (catch 'exit
    (loop for ex in examples do
	 (awhen (catch 'fix
		  (unless (and skip (instance-category ex))
		    (format t "~A: " (incf count))
		    (interactive-assign-category-label ex)))
	   (interactive-assign-category-label prior))
	 (setf prior ex))))

(defun interactive-assign-category-label (instance)
  (format t "(p)ositive (c)onditional (s)cope (t)ype (i)nversion (n)egative (g)arbage~%")
  (print-instance instance)
  (format t "> ")
  (setf (instance-category instance)
	(case (read-char)
	  (#\p 'positive)
	  (#\n 'negative)
	  (#\s 'scope)
	  (#\i 'inverse)
	  (#\c 'conditional)
	  (#\g 'garbage)
	  (#\t 'type)))
  (case (read-char)
    (#\q (read-char) (throw 'exit t))
    (#\f (read-char) (throw 'fix t))
    (#\Newline nil)
    (t nil)))

(defun dump-instances-with-categories (instances)
  (format t "~{~A (~A) ~2,20T~A (~A) ~3,20T~A (~A)~%~}"
	  (shuffle (mapcar #'(lambda (x) (token-array->string (phrase-pair-target (phrase-pair x)))) instances)
		   (mapcar #'(lambda (x) (aref (symbol-name (instance-category x)) 0)) instances))))

(defun get-label-stats (examples)
  (let ((counts (get-category-counts examples)))
    (append counts
	    (list (list 'precision 
			(coerce (/ (get-positive counts)
				   (max (get-total counts) 1))
				'float))
		  (list 'soft-precision 
			(coerce (/ (get-soft-positive counts)
				   (max (get-total counts) 1))
				'float))))))
(defun get-positive (counts)
  (second (find 'positive counts :key #'first)))

(defun get-soft-positive (counts)
  (+ (second (find 'positive counts :key #'first))
     (second (find 'conditional counts :key #'first))
     (second (find 'scope counts :key #'first))))

(defun get-total (counts)
  (apply #'+ (mapcar #'second counts)))

(defun get-category-counts (examples)
  (mapcar #'(lambda (cat)
	      (let ((count (count cat examples :key #'instance-category))
		    (len (length examples)))
		(list cat count (if (> len 0) (coerce (/ count len) 'float) 0.0))))
	  '(positive conditional scope type inverse negative garbage)))
       

(defun apply-individual-filters (instances path-threshold pmi-ceiling pmi-floor)
  (let ((results nil))
    (push (list 0 'initial (get-label-stats instances)) results)
    (push (list 1 'concept-filter (get-label-stats (filter-instances-by-concepts instances))) results)
    (push (list 1 'path-length 
		(get-label-stats 
		 (filter-instances-by-path-distance 
		  instances 
		  (aif-ret path-threshold (get-path-median instances)))))
	  results)
    (push (list 1 'pmi 
		(get-label-stats 
		 (band-pass-filter-instances-by-pmi 
		  instances
		  pmi-ceiling pmi-floor)))
	  results)
    (nreverse results)))

(defun apply-all-filters (instances path-threshold pmi-ceiling pmi-floor)
  (let ((results nil))
    (push (list 0 'initial (get-label-stats instances)) results)
    (let ((concept-filtered (filter-instances-by-concepts instances)))
      (push (list 1 'after-concept-filter (get-label-stats concept-filtered)) results)
      (let ((path-length-filtered 
	     (filter-instances-by-path-distance 
	      concept-filtered 
	      (aif-ret path-threshold (get-path-median instances)))))
	(push (list 2 'after-path (get-label-stats path-length-filtered)) results)
	(let ((pmi-filtered 
	       (band-pass-filter-instances-by-pmi
		path-length-filtered
		pmi-ceiling 
		pmi-floor)))
	  (push (list 3 'after-pmi (get-label-stats pmi-filtered)) results)
	  (cons (nreverse results)
		(sort-instances-by-extraction-score pmi-filtered)))))))

(defun diff-filters (result-rec source target)
  (let ((src (get-stats-from-rec source (car result-rec)))
	(dst (get-stats-from-rec target (car result-rec))))
    (mapcar (lambda (s d)
	      (list (first s)
		    (- (second s) (second d))
		    (- 1 (/ (third d) (third s)))))
	    src dst)))

(defun get-stats-from-rec (name rec)
  (third (find name rec :key #'first)))


;; =======================================================
;; =======================================================
;; =======================================================
;; Validate with extraction pattern instantiation
;; =======================================================
;; =======================================================
;; =======================================================

(defun validate-instantiated-instances (instance patterns &aux results)
  (let ((searcher (make-instance 'google-searcher)))
    (dolist (pattern patterns)
      (push (list pattern (get-instance+pattern-surface-hit-counts instance pattern searcher))
	    results))
    results))

(defun get-instance+pattern-surface-hit-counts (instance pattern searcher)
  (let ((surface-pairs 
	 (instance-surface-pairs (phrase-pair-source (phrase-pair instance))
				 (phrase-pair-target (phrase-pair instance)))))
    (let ((joint 
	   (loop for surface-pair in surface-pairs 
	      summing (instantiated-pattern-hit-count pattern 
						      (first surface-pair) 
						      (second surface-pair) 
						      searcher)))
	  (source
	   (loop for phrase in (remove-duplicates (mapcar #'first surface-pairs))
	      summing (instantiated-term-hit-count pattern phrase t searcher)))
	  (target 
	   (loop for phrase in (remove-duplicates (mapcar #'second surface-pairs))
	      summing (instantiated-term-hit-count pattern phrase nil searcher))))
      (list joint source target))))

(defun instantiated-term-hit-count (pattern phrase source-p searcher)
  (get-hit-count searcher
		 (generate-search-query-from-pattern
		  (instantiate-pattern-expression pattern phrase source-p))))

(defun instantiated-pattern-hit-count (pattern source target searcher)
  (get-hit-count searcher 
		 (generate-search-query-from-pattern
		  (instantiate-pattern-relation pattern source target))))

(defun generate-search-query-from-pattern (pattern)
  (let ((words (mapcar #'term->search-word (pattern-expression pattern))))
    (apply #'concatenate 'string 
	   (append (list "allintext: \"")
		   (shuffle words (repeat " " (1- (length words))))
		   (list "\"")))))

;; ================================================
;; Validate with manual descrimination patterns
;; ================================================

;; Modify to use surface forms?
;; Wildcards?

(defparameter *desireof-manual-descrimination-patterns*
    '(("wants to" "does not want to")
      ("desires" "does not desire")
      ("will" "will not")
      ("cares to" "does not care to")
      ("cares about" "does not care about")
      ("loves" "hates")))

(defun lookup-type-patterns (type)
  (let ((tid (find-ptype type)))
    (cond 
      ((eq (find-ptype "desireof") tid)
       *desireof-manual-descrimination-patterns*)
      ((eq (find-ptype "effectof") tid)
       *effectof-manual-descrimination-patterns*)
      ((eq (find-ptype "capableof") tid)
       *capableof-manual-descrimination-patterns*))))

(defun compute-average-pair-ratio-for-type (searcher type pair)
  (let ((src (phrase-pair-source pair))
	(dst (phrase-pair-target pair))
	(patterns (lookup-type-patterns type)))
    (coerce 
     (/ (loop for pattern in patterns sum
	     (/ (1+ (get-cached-hit-count searcher
					  (concatenate 
					   'string
					   "\""
					   (token-array->string src)
					   "+"
					   (first pattern)
					   "+"
					   (token-array->string dst)
					   "\"")))
		(1+ (get-cached-hit-count searcher
					  (concatenate 
					   'string
					   "\""
					   (token-array->string src)
					   "+"
					   (second pattern)
					   "+"
					   (token-array->string dst)
					   "\"")))))
	(length patterns))
     'float)))

(defun rank-by-ratio (searcher instances)
  (mapcar #'(lambda (instance)
	      (list instance 
		    (compute-average-pair-ratio-for-type searcher 
							 (relation-type instance)
							 (phrase-pair instance))))
	  instances))


;;
;; Vestigial playing with examples
;; 

(defun convert-example-set (examples instances)
  (mapcar #'(lambda (ex)
	      (list (find (string->token-array (third ex))
			  instances
			  :test #'equalp
			  :key (lambda (i) (phrase-pair-target (phrase-pair i))))
		    (fourth ex)))
	  examples))

(defun filter-example-by-list (examples list)
  (select-if (lambda (ex)
	       (member (first ex) list))
	     examples))

