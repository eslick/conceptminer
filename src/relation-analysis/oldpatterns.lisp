(in-package :conceptminer)

;; ============ ARCHIVE =============

(defun extract-patterns (query page)
  (mapcar #'window->pattern-instance
	  (extract-training-windows query page 
				    *interior-pattern-instance-width*
				    *exterior-pattern-instance-allowance*)))


;;
;; Generalizing patterns
;;

;; (defmethod edit-distance ((pa pattern) (pb pattern))
;;   (+ (expression-distance (pattern-before-expr pa) (pattern-before-expr pb))
;;      (expression-distance (pattern-first-expr pa) (pattern-first-expr pb))
;;      (expression-distance (pattern-middle-expr pa) (pattern-middle-expr pb))
;;      (expression-distance (pattern-second-expr pa) (pattern-second-expr pb))
;;      (expression-distance (pattern-after-expr pa) (pattern-after-expr pb))))

(defun most-connected-patterns (patterns)
  (let ((table (make-hash-table :size (length patterns))))
    (loop for spat in patterns do
       (loop for tpat in patterns do
	  (unless (eq spat tpat)
	    (mvbind (cost edit-sig) (smart-edit-distance spat tpat)
	      (aif (gethash spat table)
		   (progn
		     (incf (car it) cost)
		     (push (cons tpat edit-sig) (cdr it)))
		   (setf (gethash spat table) (cons 0 nil)))))))
    (sort (hash-items table) #'< :key #'cadr)))

(defmethod quick-edit-distance ((pa pattern) (pb pattern))
  (expression-distance (pattern-expression pa) (pattern-expression pb)))

(defmethod smart-edit-distance ((pa pattern) (pb pattern))
  "Normalize where the edit distance is computed from"
  (mvbind (fcost fsig) (expression-distance (pattern-expression-forward pa) 
					    (pattern-expression-forward pb))
    (mvbind (bcost bsig) (expression-distance (pattern-expression-reverse pa)
					      (pattern-expression-reverse pb))
      (values (+ fcost bcost)
	      (cons bsig fsig)))))

(defun expression-distance (expr1 expr2)
  (labels ((test-next (term expr)
	     (when (cdr expr)
	       (term-eq? term (cadr expr))))
	   (rec (e1 e2 cost sig)
	     (cond ((null e1)
		    (values cost (nreverse (cons :delete-rest sig))))
		   ((null e2)
		    (values cost (nreverse (cons :insert-rest sig))))
		   ((term-eq? (car e1) (car e2))
		    (rec (cdr e1) (cdr e2) cost (cons :accept sig)))
		   ((term-pos-eq? (car e1) (car e2))
		    (rec (cdr e1) (cdr e2) (+ cost (edit-cost :wildcard-word)) (cons :wildcard-word sig)))
		   ((test-next (car e1) e2)
		    (rec (cdr e1) (cddr e2) (+ cost (edit-cost :delete)) (cons :delete sig)))
		   ((test-next (car e2) e1)
		    (rec (cddr e1) (cdr e2) (+ cost (edit-cost :insert)) (cons :insert (cons (car e1) sig))))
		   (t (rec (cdr e1) (cdr e2) 
			   (+ cost (edit-cost :wildcard-all))
			   (cons :wildcard-all sig))))))
    (rec expr1 expr2 0 nil)))

(defun edit-cost (type)
  (case type
    (:insert 3)
    (:delete 3)
    (:wildcard-word 1)
    (:wildcard-all 2)))

(defun apply-edit-signature (pattern signature &aux (rsig (cdr signature)))
  (labels ((walk-expr (expr sig result)
	     (if (null expr)
		 (values (nreverse result) sig)
		 (case (car sig)
		   (nil (values (nreverse result) nil))
		   (:delete-rest (values (nreverse result) nil))
		   (:accept (walk-expr (cdr expr) (cdr sig) (cons (car expr) result)))
		   (:insert (walk-expr expr (cddr sig) (cons (cadr sig) result)))
		   (:wildcard-word (walk-expr (cdr expr) (cdr sig) (cons (make-term :* (term-pos (car expr))) result)))
		   (:wildcard-all (walk-expr (cdr expr) (cdr sig) (cons (make-wildcard-term) result))))))
	   (apply-to (slot &optional (before nil))
	     (mvbind (res sig) 
	         (walk-expr (slot-value pattern slot)
			    (if before (car signature) rsig)
			    nil)
	       (unless before (setf rsig sig))
	       res)))
    (make-instance 'generalized-pattern
		   :query (pattern-query pattern)
		   :page (pattern-page pattern)
		   :direction (pattern-direction pattern)
		   :before (apply-to 'before-expr t)
		   :first (apply-to 'first)
		   :middle (apply-to 'middle-expr)
		   :second (apply-to 'second)
		   :after (apply-to 'after-expr)
		   :sources (list pattern))))


;;
;; Ordering patterns - TODO
;;

(defun pattern-< (pa pb)
  "Simple function that picks some canonical order for patterns so 
   combinations can be discovered by looking locally in a list.  
   O(2m) comparisons"
  (expr-< (pattern-expression pa) (pattern-expression pb)))

(defun expr-< (expr1 expr2)
  "Equivalent expressions return false
   Shorter expressions have lesser order
   :* is least in order (more general)
   negations
   followed by boolean exprs (:or > :and)
   followed by terms
     negations < conjunctions < specific terms"
  (cond ((and (null expr1) (not (null expr2)))
	 t)
	((and (not (null expr1)) (null expr2))
	 nil)
	((and (null expr1) (null expr2))
	 nil)
	((subexpr-< (first expr1) (first expr2))
	 t)
	(t (expr-< (rest expr1) (rest expr2)))))

;;(defun subexpr-< (term1 term2))


;; ===============================================================
;; ===============================================================
;; ===============================================================

;;
;; Old bootstrap code
;;


(defun window->pattern-instance (twindow)
  (let ((vdoc (page-contents (training-window-page twindow))))
    (multiple-value-bind (before first middle second after)
	(window-region-intervals twindow)
      (make-instance 'pattern 
;;		     :relation-id (cnet-query-relation-id (training-window-query twindow))
		     :query (training-window-query twindow)
		     :page (training-window-page twindow)
		     :before (interval->pattern-expr before vdoc)
		     :first (interval->pattern-expr first vdoc :tags)
		     :middle (interval->pattern-expr middle vdoc)
		     :second (interval->pattern-expr second vdoc :tags)
		     :after (interval->pattern-expr after vdoc)
		     :direction (training-window-direction twindow)))))

