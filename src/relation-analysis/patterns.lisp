(in-package :conceptminer)

;;
;; Playing with lexico-syntactic patterns
;;

(defclass pattern ()
  ((query :accessor pattern-query :initarg :query)
   (page :accessor pattern-page :initarg :page)
   (before-expr :accessor pattern-before-expr :initarg :before)
   (first :accessor pattern-first-expr :initarg :first)
   (middle-expr :accessor pattern-middle-expr :initarg :middle)
   (second :accessor pattern-second-expr :initarg :second)
   (after-expr :accessor pattern-after-expr :initarg :after)
   (direction :accessor pattern-direction :initarg :direction)))

(defclass generalized-pattern (pattern)
  ((relation-type :accessor pattern-relation-type :initarg :type :initform nil)
   (sources :accessor pattern-sources :initarg :sources :initform nil)
   (score :accessor pattern-score :initarg :sources :initform (list 0.0 0 0))))

(defmethod relation-id ((p pattern))
  (cnet-query-relation-id (pattern-query p)))

(defun pattern-type (pattern)
  (relation-type-id
   (get-relation 
    (cnet-query-relation-id 
     (pattern-query pattern)))))

(defun pattern-typename (pattern)
  (get-ptypename (pattern-type pattern)))

(defmethod copy-pattern ((p pattern) &optional (type 'pattern))
  (make-instance type
		 :query (pattern-query p)
		 :page (pattern-page p)
		 :before (copy-list (pattern-before-expr p))
		 :first (copy-list (pattern-first-expr p))
		 :middle (copy-list (pattern-middle-expr p))
		 :second (copy-list (pattern-second-expr p))
		 :after (copy-list (pattern-after-expr p))
		 :direction (pattern-direction p)))

(defmethod copy-generalized-pattern ((p pattern))
  (copy-pattern p 'generalized-pattern))

(defmethod copy-generalized-pattern ((p generalized-pattern))
  (let ((new (copy-generalized-pattern (coerce p 'pattern))))
    (setf (pattern-relation-type new) (pattern-relation-type p))
    (setf (pattern-sources new) (pattern-sources p))
    new))

(defmethod print-pattern ((p pattern))
  (with-slots (before-expr first middle-expr second after-expr) p
    (format t "~:[b~;f~] ~{~A ~}~%" (eq (pattern-direction p) :forward)
	    (mapcar #'term-string (pattern-expression p)))))

(defmethod print-concise-pattern ((p pattern))
  (apply #'concatenate 'string
;;	 (format nil "~:[b~;f~] " (eq (pattern-direction p) :forward))
	 (let ((list (pattern-term-list p)))
	   (shuffle list (repeat " " (max (1- (length list)) 0))))))

(defmethod pattern-term-list ((p pattern))
  (append (mapcar #'term-string (pattern-before-expr p))
	  (mklist (if (eq (pattern-direction p) :forward)
		      "X/*"
		      "Y/*"))
	  (mapcar #'term-string (pattern-middle-expr p))
	  (mklist (if (eq (pattern-direction p) :forward)
		      "Y/*"
		      "X/*"))
	  (mapcar #'term-string (pattern-after-expr p))))

;;
;; Cache patterns since they aren't persistent
;;

(defvar *page-pattern-cache* (make-hash-table :size 10000))

(defun clear-pattern-cache ()
  (setf *page-pattern-cache* (make-hash-table :size 10000)))

(defun get-patterns-for-relation-id (id)
  (mapcan #'get-patterns-for-query
	  (get-queries-for-relation id)))

(defun get-patterns-for-relation-ids (id-list)
  (mapcan #'get-patterns-for-relation-id id-list))

(defun get-patterns-for-query (query)
  (mapcan (lambda (page)
;;	    (aif (gethash page *page-pattern-cache*)
;;		 (copy-list it)
		 (setf (gethash page *page-pattern-cache*)
		       (extract-pattern-sentences query page *interior-pattern-instance-width*)))
	  (get-pages-for-query query)))

;;
;; Manipulating patterns
;;

(defun make-term (word pos)
  `(:term ,word ,pos))

(defun make-wildcard-term ()
  `(:term :* :*))

(abbrevs expr-tag first
	 term-word second
	 term-pos third)

(defun check-tag (expr tag)
  (assert (eq (expr-tag expr) tag)))

(defun wildcard-term? (term)
  (or (eq term '*)
      (eq term :*)))

(defun and-term? (term)
  (or (eq term 'and)
      (eq term :and)))

(defun or-term? (term)
  (or (eq term 'OR)
      (eq term :OR)))

(defun and-expr? (expr)
  (and-term? (expr-tag expr)))

(defun or-expr? (expr)
  (or-term? (expr-tag expr)))

(defun term-expr? (expr)
  (eq (expr-tag expr) :term))

(defun expr-length (expr)
  (length expr))

(defmethod length-of ((p pattern))
  (expr-length (pattern-expression p)))

(defun interval->pattern-expr (interval vdoc &optional type)
  (range->pattern-expr (interval-start interval) (interval-end interval) vdoc type))

(defun phrase->pattern-expr (phrase)
  (range->pattern-expr (phrase-start phrase) (phrase-end phrase) (phrase-document phrase)))

(defun array->pattern-expr (array)
  "Tokens only, no POS"
  (loop for id across array collect
    (make-term id '*)))

(defun range->pattern-expr (start end vdoc &optional type)
  (loop for offset from start upto end collect
       (make-term 
	(if (eq type :tags) '*
	    (get-token-id vdoc offset))
	(if (eq type :token) '*
	    (get-tag vdoc offset)))))
      
(defun term-string (term)
  (if (term-expr? term)
      (format nil "~A/~A" (etypecase (term-word term)
			    (fixnum (token-for-id (term-word term)))
			    (symbol (term-word term)))
	      (term-pos term))
      (format nil "~A/*" term)))

;;
;; Making patterns from text
;;

(defparameter *interior-pattern-instance-width* 5)
(defparameter *exterior-pattern-instance-allowance* 30)

(defun extract-pattern-sentences (query page interior-width)
  "Extract a set of training windows from page according to the relation in query (cnet-query type)
   and where phrases are within width words of each other, including margin words on either end"
  (let ((relation (cnet-query-relation-id query)))
    (if (or (neq (type-of relation) 'fixnum) (eq relation 0))
	(warn "Relation from query ~A not a relation id or is 0" relation)
	(multiple-value-bind (forward-pairs backward-pairs)
	    (get-document-interval-pairs-for-phrases (page-contents page)
						     (relation-source-phrase relation)
						     (relation-target-phrase relation)
						     interior-width 
						     :concept-terms t)
	  (nconc (mapcar #'(lambda (interval-pair)
			     (make-pattern-from-intervals query page interval-pair :forward))
			 forward-pairs)
		 (mapcar #'(lambda (interval-pair)
			     (make-pattern-from-intervals query page interval-pair :backward))
			 backward-pairs))))))

(defun make-pattern-from-intervals (query page interval-pair direction)
  (destructure-interval-pair (startA endA startB endB) interval-pair
    (let ((sentence-start (find-eos (page-contents page) startA
				  *exterior-pattern-instance-allowance* :forward nil))
	  (sentence-end (find-eos (page-contents page) endB
				*exterior-pattern-instance-allowance* :forward t)))
      (when (and sentence-start sentence-end)
	(make-instance 'pattern
		       :query query
		       :page page
		       :before (range->pattern-expr (max sentence-start 0) (1- startA) (page-contents page))
		       :first (interval->pattern-expr (first interval-pair) (page-contents page) :tags)
		       :middle (range->pattern-expr (1+ endA) (1- startB) (page-contents page))
		       :second (interval->pattern-expr (second interval-pair) (page-contents page) :tags)
		       :after (range->pattern-expr (1+ endB)
						   (min sentence-end 
							(length-of (page-contents page)))
						   (page-contents page))
		       :direction direction)))))

(defun find-eos (vdoc start max-offset &key (forward t))
  (flet ((eos? (tag)
	   (case tag
	     (:. t)
	     ('\. t)
	     (t nil))))
    (find-tag vdoc start max-offset #'eos? :forward forward)))

(defun find-tag (vdoc start max-offset match-fn &key (forward t) &aux offset)
  (flet ((next-pos ()
	   (if forward
	       (incf offset)
	       (decf offset)))
	 (max-offset ()
	   (if forward
	       (min max-offset (1- (- (length-of vdoc) start)))
	       (min max-offset (1- start)))))
    (setf offset start)
    (loop for i from 0 upto (max-offset) do
	 (when (funcall match-fn (get-tag vdoc offset))
	   (return-from find-tag offset))
	 (next-pos))
    (if forward
	(+ start (max-offset))
	(- start (max-offset)))))
	

;;
;; Some pattern->expression ops
;;

(defmethod %pattern-expression (pattern)
  (with-slots (before-expr first middle-expr second after-expr) pattern
    (cons (reverse before-expr) (append first middle-expr second after-expr))))

(defmethod pattern-expression (pattern)
  (let ((norm-expr (%pattern-expression pattern)))
    (append (reverse (car norm-expr)) (cdr norm-expr))))

(defmethod pattern-expression-forward (pattern)
  (cdr (%pattern-expression pattern)))

(defmethod pattern-expression-reverse (pattern)
  (car (%pattern-expression pattern)))

(defun instantiate-pattern-expression (pattern phrase &optional source-not-target-p)
  (let ((new (copy-pattern pattern))
	(expr (typecase phrase
		(phrase (phrase->pattern-expr phrase))
		(array (array->pattern-expr phrase)))))
    (if source-not-target-p
	(setf (pattern-first-expr new) expr)
	(setf (pattern-second-expr new) expr))
    new))

(defun instantiate-pattern-relation (pattern source target)
  (let ((new (copy-pattern pattern)))
    (setf (pattern-first-expr new) 
	  (typecase source
	    (phrase (phrase->pattern-expr source))
	    (array (array->pattern-expr source))))
    (setf (pattern-second-expr new)
	  (typecase target
	    (phrase (phrase->pattern-expr target))
	    (array (array->pattern-expr target))))
    new))

(defun pattern-signature (pattern margin)
  "Generate a signature for a larger pattern by restricting the field of view"
  (append (reverse (awhen (reverse (pattern-before-expr pattern))
		     (subseq it 0 (min margin (length it)))))
	  (pattern-first-expr pattern)
	  (pattern-middle-expr pattern)
	  (pattern-second-expr pattern)
	  (awhen (pattern-after-expr pattern)
	    (subseq it 0 (min margin (length it))))))

;;
;; Match pattern to text 
;;

(defmethod match-pattern-expr? (expr (doc vector-document) offset)
  "Works for patterns aligned to the phrase"
  (declare (special *count*))
  (labels ((accept-first-term (term)
	     (cond ((wildcard-term? term)
		    t)
		   ((not (consp term))
		    (error "Unrecognized pattern element: ~A in ~A" term expr))
		   ((or-expr? term)
		    (aif (some (lambda (subexpr)
				 (let ((*count* *count*))
				   (declare (special *count*))
				   (if (match-pattern-expr? subexpr doc offset)
				       *count*
				       nil)))
				(rest term))
			 (setf *count* it)))
		   ((and-expr? term)
		    (every (lambda (subexpr)
			     (match-pattern-expr? subexpr doc offset))
			   (rest term)))
		   ((term-expr? term)
		    (match-term? term (get-token-id doc offset) (get-tag doc offset)))
		   (t (error "Unhandled pattern ~A in ~A" term expr)))))
    (when (accept-first-term (first expr))
      (cond ((null (cdr expr)) 
	     t)
	    ((>= offset (1- (length-of doc)))
	     nil)
	    (t 
	     (match-pattern-expr? (cdr expr) doc (1+ offset)))))))

(defun match-term? (expr word pos)
  "Match a term entry to a word/pos pair"
  (declare (special *count*))
  (check-tag expr :term)
  (when (and (match-word? (term-word expr) (get-lemma-for-id word))
	     (match-pos? (term-pos expr) pos))
    (incf *count*)))

(defun match-word? (expr word)
  (cond ((eq expr '*)
	 t)
	((consp expr)
	 (some #'(lambda (subexpr)
		   (match-word? subexpr word))
	       expr))
	((numberp expr)
	 (eq expr word))))

(defun match-pos? (expr pos)
  (cond ((eq expr '*) ;; wildcard
	 t)
	((consp expr) ;; conjunctions
	 (member pos expr))
	((eq pos :V)  ;; special conjunctions
	 (verb-pos? pos))
	((eq pos :N)
	 (noun-pos? pos))
	((symbolp pos) ;; default
	 (eq expr pos))
	(t (error "Unknown case in match-pos? ( ~A ~A )" expr pos))))

(defun term-pos-eq? (term1 term2)
  (match-pos? (term-pos term1) (term-pos term2)))

(defun term-eq? (term1 term2)
  (and (term-pos-eq? term1 term2)
       (match-word? (term-word term1) (term-word term2))))



;;
;; Finding patterns in text
;;

(defmethod find-pattern-offset ((p pattern) vdoc &optional (start 0) (end (length-of vdoc)))
  "Find pattern in text"
  (loop for offset from start below end do
       (let ((*count* 0))
	 (declare (special *count*))
	 (awhen (match-pattern-expr? (pattern-expression p) vdoc offset)
	   (return (values offset *count*))))
       finally (return nil)))

(defmethod find-pattern-instances ((p pattern) vdoc &optional (start 0) (end (length-of vdoc)))
  "Find all instances of a pattern in text"
  (when vdoc
    (multiple-value-bind (offset length) (find-pattern-offset p vdoc start end)
      (when offset
	(cons (make-phrase-from-vdoc vdoc offset length :pattern-instance)
	      (find-pattern-instances p vdoc (1+ offset)))))))

(defmethod get-pattern-instance-source ((pattern pattern) (instance phrase))
  (let* ((start (+ (phrase-start instance)
		  (pattern-source-offset pattern)))
	 (len (length (find-matching-subexpression (pattern-first-expr pattern) instance start))))
    (phrase->token-array (make-phrase-from-vdoc (phrase-document instance) start len :concept-instance))))

(defun pattern-source-offset (pattern)
  (length (pattern-before-expr pattern)))

(defmethod get-pattern-instance-target ((pattern pattern) (instance phrase))
  (let* ((start (+ (phrase-start instance)
		   (pattern-target-offset pattern instance)))
	 (len (length (find-matching-subexpression (pattern-second-expr pattern) instance start))))
    (phrase->token-array (make-phrase-from-vdoc (phrase-document instance) start len :concept-instance))))

(defun pattern-target-offset (pattern instance)
  (+ (length (pattern-before-expr pattern))
     (length (find-matching-subexpression (pattern-first-expr pattern) instance (phrase-start instance)))
     (length (pattern-middle-expr pattern))))

(defun find-matching-subexpression (expr instance start)
  (let ((*count* 0))
    (declare (special *count*))
    (cond ((null expr)
	   (error "Expression must not be empty: ~A" expr))
	((term-expr? (first expr)) 
	 expr)
	((or-expr? (first expr))
	 (find instance (cdar expr) 
	       :test #'(lambda (inst expr) (match-pattern-expr? expr (phrase-document inst) start)))))))

;;
;; PATTERN EXTRACTION API
;;

(defparameter *score-degrade-limit* 0.5)
(defparameter *score-threshold* 0.01)

(defun extract-general-patterns (sentence-patterns number)
  "This runs the whole process of building a table of common patterns by:
   * canonicalizing
   * adding to table using middle expressions
   * scoring using (unique% log size) metric
   * generalizing by exploring before/after tokens on high scoring patterns
   * pick top patterns according to parameters
   "
  (let ((table (build-common-pattern-table-with-tokens sentence-patterns)))
    (expand-pattern-table table *score-degrade-limit* *score-threshold*)
    (values (top-general-patterns table number) table)))

(defun top-general-patterns (table number)
  (loop for entry in (select-top-table-entries table number *score-threshold*) collect
       (consolidate-pattern-group (entry-patterns entry) (entry-params entry) (entry-score entry))))


;;
;; Cannonicalize patterns
;;

(defun canonicalize-pattern-expr (expr)
  (on-cdrs (cond ((verb-pos? (term-pos it))
		  (cons (make-term (term-word it) :V) rec))
		 ((punctuation? (term-pos it))
		  rec)
		 ((eq (term-pos it) :DT)
		  rec)
		 (t (cons it rec)))
	   nil
	   expr))

(defun canonicalize-pattern (pattern)
  (with-slots (before-expr first middle-expr second after-expr) pattern
    (setf before-expr (canonicalize-pattern-expr before-expr))
    (setf first (canonicalize-pattern-expr first))
    (setf middle-expr (canonicalize-pattern-expr middle-expr))
    (setf second (canonicalize-pattern-expr second))
    (setf after-expr (canonicalize-pattern-expr after-expr)))
  pattern)

;;
;; Group common patterns
;;


(defun build-common-pattern-table (list &optional (before-len 0) (after-len 0))
  "Count the number of identical patterns and return a sorted list, highest to lowest"
  (let ((table (make-hash-table :size (length list) :test #'equal)))
    (dolist (pattern list)
      (canonicalize-pattern pattern)
      (let ((before (reverse (pattern-before-expr pattern)))
	    (after (pattern-after-expr pattern)))
	(let ((sig (append 
		    (when before (subseq before 0 (min before-len (length before))))
		    (when (> before-len 0) (mklist :first))
		    (pattern-middle-expr pattern)
		    (when (> after-len 0) (mklist :second))
		    (when after (subseq after 0 (min after-len (length after)))))))
	  (aif (gethash sig table)
	       (progn
		 (incf (car it))
		 (push pattern (cddr it)))
	       (setf (gethash sig table)
		     (list 1 `(,before-len ,after-len) pattern))))))
    (compute-common-pattern-scores table)))

(defun build-common-pattern-table-with-tokens (list &optional (before-len 0) (after-len 0))
  (let ((table (make-hash-table :size (length list) :test #'equal)))
    (dolist (pattern list)
      (canonicalize-pattern pattern)
      (let* ((before (reverse (pattern-before-expr pattern)))
	     (after (pattern-after-expr pattern))
	     (sig 
	      (append 
	       (when before (pattern-expr-lexical-terms (subseq before 0 (min before-len (length before)))))
	       (when (> before-len 0) (mklist :first))
	       (pattern-expr-lexical-terms (pattern-middle-expr pattern))
	       (when (> after-len 0) (mklist :second))
	       (when after (pattern-expr-lexical-terms (subseq after 0 (min after-len (length after))))))))
	(aif (gethash sig table)
	     (progn 
	       (incf (car it))
	       (push pattern (cddr it)))
	     (setf (gethash sig table)
		   (list 1 `(,before-len ,after-len) pattern)))))
    (compute-common-pattern-scores table)))

(defun pattern-expr-lexical-terms (expr)
  (loop for term in expr collect
       (term-word term)))

;;
;; Assign scores to patterns
;;

(defun compute-common-pattern-scores (table)
  "Given a table, compute a score which is (* (/ unique total) (log total))
   which balances precision vs. recall on the seed instances"
  (maphash (lambda (k entry)
	     (declare (ignore k))
	     (dbind (total-patterns params . patterns) entry
	       (declare (ignore params))
	       (let ((total-unique (count-unique-patterns patterns)))
		 (setf (car entry)
		       (list (percent-unique-log-count-score total-patterns total-unique)
			     ;; (percent-unique-log-count-score total-patterns total-unique)
			     total-patterns
			     total-unique)))))
	   table)
  table)

(defun count-unique-patterns (patterns)
  (length (remove-duplicates (mapcar #'relation-id patterns))))

(defun unique-count-score (total unique)
  (declare (ignore unique))
  unique)

(defun percent-unique-log-count-score (total unique)
  (* (log total)
     (/ unique total)))

(defun sort-pattern-table-by-score (table)
  (sort (hash-items table) 
	(lambda (a b)
	  (typecase a
	    (fixnum (> a b))
	    (cons (> (first a) (first b))) ))
	:key #'cadr))

(defun sort-pattern-table-by-unique-count (table)
  (sort (hash-items table)
	(lambda (a b)
	  (typecase a
	    (fixnum (> a b))
	    (cons (> (third a) (third b)))))
	:key #'cadr))

;;
;; Explore more specific patterns
;;

(defun expand-pattern-table (table &optional (limit 0.5) (threshold 1))
  (awhen (find-more-specific-patterns table limit threshold)
    (dolist (entry it)
      (setf (gethash (car entry) table) (cdr entry))))
  table)

(defun find-more-specific-patterns (table degrade-limit threshold &aux new)
  "For high scoring patterns, see if we can make the pattern more specific without
   significantly degrading the score.  Degrade-limit is a percentage and threshold
   is an absolute unique-log-count score"
  (loop for entry in (sort-pattern-table-by-score table) do
       (loop for pair in (combinations '(0 1 2 3) '(0 1 2 3)) do
	    (dolist (entry (try-pattern-expansion (entry-patterns entry) (first pair) (second pair)
						  threshold degrade-limit (entry-score entry)))
	      (push entry new))))
  (sort new #'> :key #'entry-score))

(defun try-pattern-expansion (patterns left right threshold degrade-percent current-score)
  "Incrementally expand patterns in order to see if more 
   specific patterns have better scores"
  (remove-nulls 
   (loop for entry in 
	(sort-pattern-table-by-score
	 (build-common-pattern-table-with-tokens patterns left right))
      collecting
	(when (and (> (entry-score entry) threshold)
		   (> (entry-score entry) (* current-score (- 1 degrade-percent))))
	  entry))))

;;
;; Ranking
;;

(defun select-top-table-entries (table total threshold &optional (perfect 2) &aux results)
  (dolist (entry (sort-pattern-table-by-score table))
    (when (or (> (entry-score entry) threshold)
	      (and (> perfect 0) 
		   (= (entry-total entry) (entry-unique entry)) 
		   (<= perfect (entry-total entry))))
      (push entry results)
      (when (<= (decf total) 0)
	(return-from select-top-table-entries (nreverse results)))))
  (nreverse results))


;;
;; Pattern table utilities
;;

(defun entry-score (entry)
  (caadr entry))

(defun entry-total (entry)
  (second (cadr entry)))

(defun entry-unique (entry)
  (third (cadr entry)))

(defun entry-params (entry)
  (caddr entry))

(defun entry-patterns (entry)
  (cdddr entry))

(defun expr-string (expr)
  (apply #'concatenate 'string 
	 (shuffle (mapcar #'term-string expr)
		  (repeat " " (max 0 (1- (length expr)))))))

(defun dump-top-patterns (table count)
  (mapcar #'(lambda (entry)
	      (awhen (first entry)
		(list (expr-string it) (second entry))))
	  (subseq (sort-pattern-table-by-score table) 0 count)))


(defun patterns-for-nth-ranked-entry (entries n)
  (entry-patterns (nth n entries)))

(defun dump-nth-top-pattern-sources (table n)
  (mapcar #'(lambda (p) (print-pattern p))
	  (patterns-for-nth-ranked-entry (sort-pattern-table-by-score table) n)))

(defun print-pattern-relations (patterns)
  (dolist (p patterns)
    (print-relation (get-relation (relation-id p)))))

;;
;; Consolidate patterns in pattern-table
;;

(defun consolidate-pattern-group (patterns params score)
  "Return a new pattern that consolidates the contributions
   of the others under params.  Capture all the sources for
   backtracking purposes."
  (labels ((rec (list pattern)
	     (cond ((null list) pattern)
		   (t (push (car list) (pattern-sources pattern))
		      (merge-instance-slots (car list) pattern)
		      (rec (cdr list) pattern)))))
    (cond ((not (listp patterns))
	   nil)
	  ((= (length patterns) 1)
	   (car patterns))
	  ((> (length patterns) 2)
	   (rec (cdr patterns)
		(copy-generalized-pattern-with-params (car patterns) params score))))))

(defun copy-generalized-pattern-with-params (pattern params score)
  (let ((new (copy-generalized-pattern pattern)))
    (setf (pattern-before-expr new)
	  (when (pattern-before-expr new)
	    (nreverse (subseq (nreverse (pattern-before-expr new)) 0 (first params)))))
    (setf (pattern-after-expr new)
	  (when (pattern-after-expr new)
	    (subseq (pattern-after-expr new) 0 (second params))))
    (setf (pattern-score new) score)
    new))

(defun merge-instance-slots (pattern-source pattern-target)
  (symbol-macrolet 
      ((source-first (pattern-first-expr pattern-source))
       (source-second (pattern-second-expr pattern-source))
       (target-first (pattern-first-expr pattern-target))
       (target-second (pattern-second-expr pattern-target)))
    (setf target-first (list (ensure-or (car target-first))))
    (setf target-second (list (ensure-or (car target-second))))
    (when (and source-first target-first 
	       (not (member source-first (cdar target-first) :test #'equal)))
      (setf target-first (insert-or-expression target-first source-first)))
    (when (and source-second target-second 
	       (not (member source-second (cdar target-second) :test #'equal)))
      (setf target-second (insert-or-expression target-second source-second)))
    pattern-target))

(defun insert-or-expression (or-expr expr)
  (push expr (cdar or-expr))
  (setf (cdar or-expr)
	(sort (cdar or-expr) #'(lambda (a b) (> (length a) (length b)))))
  or-expr)

(defun or-terms (expr)
  (assert (eq (first expr) :or))
  (cdr expr))

(defun ensure-or (expr)
  (if (or-expr? expr)
      expr
      `(:or ,(list expr))))

;;
;; Final scoring/ranking
;;

;; compute PMI of pattern and instances?
;; recall statistics against heavy relations
;; simple total counts (i.e. good at extracting instances of the seed class?)
;; need to account for false negatives!



