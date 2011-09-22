;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: eventminer -*-

(in-package :conceptminer)

;; naive bayes classification for C categories

;; A classifier has a set of Categories 'C' which are trained against
;; a corpus of evidence 'E' where each piece of evidence consists of
;; a category C_n and a set of features f_n.  We can add more structure
;; to this later, such as ordering, tenses, etc.  For now we stick to
;; a bag of POS tags and words.

;; A training set results in a trained classifier having;; - Total samples (universe U)
;; - probability of a word or tag given all samples
;; - conditional probability of ...

(defun classify (text type)
  (classification (score (extract-features text type) type)))

;;;; ----------------------------
;;;; Data structures
;;;; ----------------------------

(defclass word-feature ()
  ((word       
    :initarg :word
    :accessor word
    :initform (error "Must supply :word")
    :documentation "The word this feature represents.")
   (is-count
    :initarg :is-count
    :accessor is-count
    :initform 0
    :documentation "Number of is's we have seen this feature in.")
   (is-not-count
    :initarg :is-not-count
    :accessor is-not-count
    :initform 0
    :documentation "Number of is-not's we have seen this feature in.")))

(defclass labeled-sentence ()
  ((sentence
    :initarg :sentence
    :accessor sentence
    :initform ""
    :documentation "The sentence associated with this labeled sentence.")
   (label
    :initarg :label
    :accessor label
    :initform ""
    :documentation "The label associated with this labeled sentence.")))

(defclass binary-classifier ()
  ((feature-database
    :initarg :feature-database
    :accessor feature-database
    :initform (make-hash-table :test #'equal)
    :documentation "A hash table containing word-feature objects")
   (total-is-count
    :initarg :total-is-count
    :accessor total-is-count
    :initform 0
    :documentation "Total number of positive samples seen by this classifier")
   (total-is-not-count
    :initarg :total-is-not-count
    :accessor total-is-not-count
    :initform 0
    :documentation "Total number of negative samples seen by this classifier")
   (correct
    :initarg :correct
    :accessor correct
    :initform 0
    :documentation "The number of samples classified correctly using this classifier")
   (incorrect
    :initarg :incorrect
    :accessor incorrect
    :initform 0
    :documentation "The number of samples classified incorrectly using this classifier")
    (total
    :initarg :total
    :accessor total
    :initform 0
    :documentation "The total number of samples classified using this classifier")))

(defclass bayesian-score-extractor ()
  ((binary-classifier-database
    :initarg :binary-classifier-database
    :accessor binary-classifier-database
    :initform (make-hash-table :test #'equal)
    :documentation "A hash table containing binary classifiers")
   (training-type-distro
    :initarg :training-type-distro
    :accessor training-type-distro
    :initform (make-hash-table :test #'equal)
    :documentation "A hash table: type => number of positive samples of that type")
   (valid-classes
    :initarg :valid-classes
    :accessor valid-classes
    :initform (list)
    :documentation "A list of valid class/type names in string format")
   (predicate-filename
    :initarg :predicate-filename
    :accessor predicate-filename
    :initform ""
    :documentation "Path and filename of the refined predicates file in string format")
   (omcs-filename
    :initarg :omcs-filename
    :accessor omcs-filename
    :initform ""
    :documentation "Path and filename of the omcs raw id file in string format")))

(defclass euclidean-classifier ()
  ((training-type-vector-score
    :initarg :training-type-vector-score
    :accessor training-type-vector-score
    :initform (make-hash-table :test #'equal)
    :documentation "A hash table containing scores of vector types")
   (training-type-counter
    :initarg :training-type-counter
    :accessor training-type-counter
    :initform (make-hash-table :test #'equal)
    :documentation "A hash table containing number of samples for each type")
   (correct
    :initarg :correct
    :accessor correct
    :initform 0
    :documentation "The number of samples classified correctly using this classifier")
   (incorrect
    :initarg :incorrect
    :accessor incorrect
    :initform 0
    :documentation "The number of samples classified incorrectly using this classifier")
    (total
    :initarg :total
    :accessor total
    :initform 0
    :documentation "The total number of samples classified using this classifier")))

(defvar *bayes-set* (make-instance 'bayesian-score-extractor 
		      :valid-classes (list "ISA" "CAPABLEOF" "EFFECTOF" "LOCATIONOF" "CAPABLEOFRECEIVINGACTION" "MOTIVATIONOF" "DESIREOF" "PROPERTYOF" "USEDFOR" "SUBEVENTOF" "PARTOF" "DESIROUSEFFECTOF")
		      :predicate-filename "C:/refined_predicates.txt" 
		      :omcs-filename "C:/omcsraw_id.txt"))

(defvar *euc-class* (make-instance 'euclidean-classifier))

(defun get-binary-classifier (type)
  (gethash type (binary-classifier-database *bayes-set*)))

(defun clear-bayes-set ()
  (setq *bayes-set* (make-instance 'bayesian-score-extractor 
		      :valid-classes (list "ISA" "CAPABLEOF" "EFFECTOF" "LOCATIONOF" "CAPABLEOFRECEIVINGACTION" "MOTIVATIONOF" "DESIREOF" "PROPERTYOF" "USEDFOR" "SUBEVENTOF" "PARTOF" "DESIROUSEFFECTOF")
		      :predicate-filename "C:/refined_predicates.txt"
		      :omcs-filename "C:/omcsraw_id.txt")))

;;;------------------------------------
;;; Database and Hash Table Tools
;;;------------------------------------

(defun init-hash (hash)
  (setq hash (make-hash-table :test #'equal)))

(defun set-hash-value (key hash val)
  (setf (gethash key hash) val))

(defun check-hash-item (key hash init-value)
  (or (gethash key hash)
      (set-hash-value key hash init-value)))

(defmethod print-object ((object word-feature) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (word is-count is-not-count) object
      (format stream "~s :is ~d :is-not ~d" word is-count is-not-count))))

(defmethod print-labeled-sentence ((object labeled-sentence) stream)
  (format t "Sentence: ~A~%Label: ~A~%" (sentence object) (label object)))

(defun print-hash (hash &optional (c 5))
  (let ((count 0))
    (with-hash-table-iterator (my-iterator hash)
      (loop
	(multiple-value-bind (entry-p k v)
	    (my-iterator)
	  (if entry-p
	      (progn		 
		(format t "~a => ~a~%" k v)
		(incf count)
		(when (<= c count) 
		  (return)))
	    (return)))))))

(defun extract-words (text)
  (delete-duplicates
   (cl-ppcre:all-matches-as-strings "[a-zA-Z]{3,}" text)
   :test #'string=))

(defun extract-features (text type)
  (mapcar (curry #'intern-feature type) (extract-words text)))

(defun intern-feature (type word)
  (check-hash-item type (binary-classifier-database *bayes-set*) (make-instance 'binary-classifier))
  (check-hash-item word (feature-database (gethash type (binary-classifier-database *bayes-set*))) (make-instance 'word-feature :word word)))


;;;; ------------------------------------------------------------------------------------------------
;;;; Training weights over words (differentiating between same words associated with different types)
;;;; ------------------------------------------------------------------------------------------------

(defun train (text type typo)
  (dolist (feature (extract-features text typo))
    (increment-count feature type typo))
  (increment-total-count type typo))

(defun increment-count (feature type typo)
  (if (string-equal type "is-not")
      (incf (is-not-count (gethash (word feature) (feature-database (get-binary-classifier typo)))))
    (incf (is-count (gethash (word feature) (feature-database (get-binary-classifier typo)))))))

(defun increment-total-count (type typo)
  (if (string-equal type "is-not")
      (incf (total-is-not-count (get-binary-classifier typo)))
    (incf (total-is-count (get-binary-classifier typo)))))

(defun is-probability (word-object type)
  (with-slots (is-count is-not-count) word-object
    (let ((is-frequency (/ is-count (max 1 (total-is-count (get-binary-classifier type)))))
          (is-not-frequency (/ is-not-count (max 1 (total-is-not-count (get-binary-classifier type))))))
      (/ is-frequency (+ is-frequency is-not-frequency)))))

(defun bayesian-is-probability (word type assumed-probability weight)
  (let ((basic-probability (is-probability word type))
        (data-points (+ (is-count word) (/ (* (is-not-count word) (total-is-count (get-binary-classifier type))) (total-is-not-count (get-binary-classifier type))))))
    (/ (+ (* weight assumed-probability)
          (* data-points basic-probability))
       (+ weight data-points))))

;;;; -------------------------------
;;;; classifying and scoring for binary classifier
;;;; --------------------------------

(defparameter *max-is-not-score* 0.4)
(defparameter *min-is-score* 0.6)

(defun classification (score)
  (progn
   (cond
     ((<= score *max-is-not-score*) "is-not")
     ((>= score *min-is-score*) "is")
     (t "unsure"))
   ))

(defun score (features type)
  (let ((is-probs ()) (is-not-probs ()) (number-of-probs 0))
    (dolist (feature features)
      (unless (untrained-p feature)
        (let ((is-prob (float (bayesian-is-probability feature type 1/2 1) 0.0d0)))
          (push is-prob is-probs)
          (push (- 1.0d0 is-prob) is-not-probs)
          (incf number-of-probs))))
    (let ((h (- 1 (fisher is-probs number-of-probs)))
          (s (- 1 (fisher is-not-probs number-of-probs))))
      (/ (+ (- 1 h) s) 2.0d0))))

(defun untrained-p (word-feature)
  (with-slots (is-count is-not-count) word-feature
    (and (zerop is-count) (zerop is-not-count))))

;;;;-------------------------------------------------
;;;;I/O Stuff
;;;;-------------------------------------------------

;;;-----------------
;;;Global Read
;;;-----------------

(defvar *global-sentences* (make-hash-table))

(defun read-sentences ()
  (progn
    (setq *global-sentences* (make-hash-table))
    (with-open-file (stream (predicate-filename *bayes-set*) :direction :input)
      (let ((id nil) (typo nil))
	(loop 
	  (when (not (setq mylist (read stream nil)))
	    (return))
	  (progn 
	    (setq id (parse-integer (car (last mylist)) :start 3))
	    (setq typo (symbol-name (car mylist)))
	    (when (valid-typo (valid-classes *bayes-set*) typo)
	      (set-hash-value id *global-sentences* (make-instance 'labeled-sentence :label typo)))))))
    (with-open-file (stream (omcs-filename *bayes-set*) :direction :input)
      (loop
	(when (not (setq mystring (read-line stream nil)))
	  (return))
	 (when (> (length mystring) 0)
	   (progn
	     (setq id (parse-integer mystring :end (position #\Space mystring)))
	     (when (gethash id *global-sentences*)
	       (setf (sentence (gethash id *global-sentences*)) (subseq mystring (1+ (position #\Space mystring)))))))))))

(defun get-training-type-distro ()
  (progn
    (maphash #'(lambda (k v)
		 (progn
		   (check-hash-item (label v) (training-type-distro *bayes-set*) 0)
		   (incf (gethash (label v) (training-type-distro *bayes-set*))))) *global-sentences*)))

(defun valid-typo (valid-list candidate)
  (let ((result nil))
    (dolist (case valid-list)
      (setq result (or result (string-equal case candidate)))) result))

;;;----------
;;;Local Read
;;;----------

(defun train-samples (typo)
  (progn
    (maphash #'(lambda (k v)
		   (train (sentence v) (translate-typo typo (label v)) typo)) *global-sentences*)))

(defun translate-typo (typo type)
  (if (string-equal type typo)
      "is"
    "is-not"))

;;;;---------------------------------------------------
;;;;Testing

(defun score-run-samples (typo)
  (let ((this-score nil))
    (progn
      (maphash #'(lambda (k v)
		     (progn
		      (setq this-score (score (extract-features (sentence v) typo) typo))
		      ;;(format t "score:=>~d type ~A label ~A~%" this-score (translate-typo typo (label v)) (classification this-score))
		       (if (string-equal (translate-typo typo (label v)) (classification this-score))
			   (progn
			     ;;(format t "Correct; old_value ~d" (correct (get-binary-classifier typo)))
			     (incf (correct (get-binary-classifier typo)))
			     ;;(format t " new_value ~d~%" (correct (get-binary-classifier typo)))
			     )
			   (progn
			     ;;(format t "Incorrect; old_value ~d" (incorrect (get-binary-classifier typo)))
			   (incf (incorrect (get-binary-classifier typo)))
			   ;;(format t " new_value ~d~%" (incorrect (get-binary-classifier typo)))
			   ))
		       (incf (total (get-binary-classifier typo))))) *global-sentences*))))

(defun print-run-results (typo)
  (let ((type nil) (total 0))
    (setq type (string-upcase typo))
    (format t "~%Run for ~A~%Correct:   ~5$~%Incorrect: ~5$~%Samples: ~d~%" type (/ (correct (get-binary-classifier typo)) (total (get-binary-classifier typo))) (/ (incorrect (get-binary-classifier typo)) (total (get-binary-classifier typo))) (total (get-binary-classifier typo)))))


;-----------------------------------------------
;All in one
(defun do-all ()
  (progn 
    (do-binary)
    (format t "~%================================================~%")
    (do-euclidean)))

(defun do-binary ()
  (progn 
    (clear-bayes-set)
    (read-sentences)
    (get-training-type-distro)
    (dolist (typo (valid-classes *bayes-set*))
      (train-samples typo)
      (score-run-samples typo)
      (print-run-results typo))))

(defun do-euclidean ()
  (progn
    (format t "training...~%")
    (train-euclidean-classifier *bayes-set* *euc-class*)
    (format t "testing...~%")
    (run-euclidean-classifier *bayes-set* *euc-class*)
    (print-n-pass-results *euc-class*)))

(defun euclidean-dist (hash hash1)
  (let ((dist 0))
    (if (not (= (hash-table-count hash) (hash-table-count hash1)))
	+maximum-positive-fixnum+
      (progn
	(maphash #'(lambda (k v)
		     (setq dist (+ dist (* (- v (gethash k hash1)) (- v (gethash k hash1)))))) hash)
	dist))))

(defun in-training-set (id)
  (if (evenp id)
      t
    NIL))


(defun score-sentence (sentence typo type euc-classer)
  ;;(format t "component ~A: ~d~%" type (score (extract-features sentence type) type))
  (check-hash-item type (check-hash-item typo (training-type-vector-score euc-classer) (make-hash-table :test #'equal)) 0)
  (set-hash-value type (gethash typo (training-type-vector-score euc-classer))
		  (+ (gethash type (gethash typo (training-type-vector-score euc-classer)))
		     (score (extract-features sentence type) type))))

(defun train-euclidean-classifier (bayesian-extractor euc-classer)
  (setf (training-type-counter euc-classer) (make-hash-table :test #'equal))
  (setf (training-type-vector-score euc-classer) (make-hash-table :test #'equal))
  ;;(setq v (gethash 22 *global-sentences*))
  (maphash #'(lambda (k v)
	       (when (in-training-set k)
		 ;;(format t "Senten: ~A; Label: ~A~%" (sentence v) (label v))
		 (check-hash-item (label v) (training-type-counter euc-classer) 0)
		 (incf (gethash (label v) (training-type-counter euc-classer)))
		 (dolist (type (valid-classes bayesian-extractor))
		   (progn
		     ;;(format t "vector component ~A:~%" type)
		     (score-sentence (sentence v) (label v) type euc-classer))))) *global-sentences*)
;; do scroe average
  (maphash #'(lambda (k v)
	       (maphash #'(lambda (l u)
	       (set-hash-value l (gethash k (training-type-vector-score euc-classer)) (/ u v)))
(gethash k (training-type-vector-score euc-classer)))
	       )
	       (training-type-counter euc-classer))
)

(defun run-euclidean-classifier (bayesian-extractor euc-classer)
  (let ((score-hash (make-hash-table :test #'equal)) (min-dist 100000000) (target-type "") (classified-type ""))
    (maphash #'(lambda (k v)
		 (when (not (in-training-set k))
		   (progn
		     (setq target-type (label v))
		     (dolist (type (valid-classes bayesian-extractor))
		       (set-hash-value type score-hash (score (extract-features (sentence v) type) type)))
		     (setq min-dist 100000000)
		     (maphash #'(lambda (l u)
				  (when (> min-dist (euclidean-dist score-hash u))
				    (setq min-dist (euclidean-dist score-hash u) classified-type l))
				  ) (training-type-vector-score euc-classer))
		     (if (string-equal classified-type target-type)
			 (incf (correct euc-classer))
		       (incf (incorrect euc-classer)))
		     (incf (total euc-classer))))) *global-sentences*)))

(defun print-n-pass-results (euc-classer)
  (format t "Total correct:   ~5$~%Total incorrect: ~5$~%" (/ (correct euc-classer) (total euc-classer)) (/ (incorrect euc-classer) (total euc-classer))))

;;;(defun check-global-sample-scores ()
;;;  (maphash #'(lambda (k v)
;;;	       (progn
;;;		 (dolist (typo (valid-classes *bayes-set*))
;;;		   (check-hash-item typo (cadr v) 0)))) *global-sample*))

;(defun classify-in-n-space ()
;  (format t "classify-in-n-space~%"))

;;;; ---------------------------------------------------
;;;; FISHER Algorithm for determining the probability
;;;;   a distribution diverges from random

(defun fisher (probs number-of-probs)
  "The Fisher computation described by Robinson."
  (inverse-chi-square 
   (* -2 (log-of-product probs))
   (* 2 number-of-probs)))

;; Ensure no underflow when multiplying probabilities

(defun decoded (float &optional (exponent 0))
  (multiple-value-bind (signif exp sign) (decode-float float)
    (values (* signif sign) (+ exp exponent))))

(defun decoded-product (significand exponent float)
  (multiple-value-bind (s e) (decoded float exponent)
    (decoded (* significand s) e)))

(defun log-of-decoded (significand exponent)
  (+ (log significand) (* exponent (log (float-radix significand)))))

(defun log-of-product (numbers)
  (loop with significand = 1.0
        with exponent    = 0
        for number in numbers do
        (multiple-value-setq (significand exponent)
          (decoded-product significand exponent number))
        finally (return (log-of-decoded significand exponent))))

(defun inverse-chi-square (value degrees-of-freedom)
  "Probability that chi-square >= value with given degrees-of-freedom.
   Based on Gary Robinson's Python implementation."
  (assert (evenp degrees-of-freedom))
  ;; Due to rounding errors in the multiplication and exponentiation
  ;; the sum computed in the loop may end up a shade above 1.0 which
  ;; we can't have since it's supposed to represent a probability.
  (min 
   (loop with m = (/ value 2)
      for i below (/ degrees-of-freedom 2)
      for prob = (exp (- m)) then (* prob (/ m i))
      summing prob)
   1.0))