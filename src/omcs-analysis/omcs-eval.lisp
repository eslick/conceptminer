;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: eventminer -*-
;;
;; Infrastructure for training classifiers over omcs database 
;; 

(in-package :conceptminer)

;; NOTE: All sequences are lists of tokens (ie head, tail, etc)

;; 
;; Utilities for analyzing classifiers over the omcs db
;; 

;; (defun load-omcs-for-study (filename index idhash &key (type :normal) (max-lines nil) &aux docs)
;;   "Returns a list of vector docs representing the sentences.
;;    If an index is passed in we update the index to point to vdocs
;;    by token ids and words"
;;   (declare (ignore type))
;;   (with-open-file (stream filename :direction :input)
;;     (do-count-contentful-lines (line count stream)
;;       (mvbind (id start) (read-from-string line)
;; 	(push (vector-tag-tokenized (subseq line start) 
;; 				    :end-tokens (mklist (id-for-token "."))) docs)
;; 	(let ((doc (first docs))
;; 	      (events (associate-concepts (get-event-chunks (first docs))))
;; 	      (nouns (associate-concepts (get-nx-chunks (first docs)))))
;; 	  (set-annotation doc :omcsid id)
;; 	  (set-annotation doc :event-chunks events)
;; 	  (set-annotation doc :np-chunks nouns)
;; 	  (setf (gethash id idhash) doc)
;; 	  (when index
;; 	    (update-index index doc (append (cdrs events) (cdrs nouns))))
;; 	  (when (and max-lines (>= count max-lines))
;; 	    (return))
;; 	  (when (= (mod count 1000) 0)
;; 	    (format t "Read ~A lines~%" count))
;; 	  (when (= (mod count 10000) 0)
;; 	    (port:gc))))))
;;   docs)

;; (defun get-lemmatized-word (word-string)
;;   (get-lemma-for-id (id-for-token word-string)))

;; (defun get-lemmatized-sentence (string)
;;   (mapcar #'get-lemmatized-word
;; 	  (extract-words string)))

;; ;; Utilities for getting sentences from the index

;; (defmethod find-sentences-from-concepts ((index index) (head concept) &optional tail)
;;   (index-into-records index (list head tail))) 


;; Generating feature sets from sentences and known constituent phrases (training on open text)

(defun make-template-feature-space ()
  (make-instance 'feature-space :dimensions 3))

(defmethod get-template-feature-vector-id ((fspace feature-space) token pos location)
  (assert (and (integerp token) (symbolp pos) (symbolp location)))
  (get-feature-vector-id fspace token pos location))

(defmethod get-template-feature-vector ((fspace feature-space) id)
  (get-feature-vector fspace id))

(defun make-cnet-class-space ()
  (make-instance 'feature-space :dimensions 2))

(defmethod get-cnet-class-vector-id ((cspace feature-space) predicate direction)
  "Take a string or integer rep of a predicate and a symbol
   for the direction of the predicate from one of (:fwd :bck)
   and return a class integer"
  (assert (and (symbolp direction) (find direction '(:fwd :bck))))
  (get-feature-vector-id cspace 
    (list (cond ((stringp predicate)
		 (get-type-id predicate))
		((integerp predicate)
		 predicate)
		(t (error "Inappropriate type for value ~A provided as predicate" predicate)))
	  direction)))

(defmethod get-cnet-class-vector ((cspace feature-space) id)
  (get-feature-vector cspace id))

(defun extract-order-and-features (vdoc phrase1 phrase2 feature-space)
  "Given a sentence as a vector document and two phrases known to exist in it, 
   find the phrases and return a list of features as triples"
  (when (and phrase1 phrase2)
    (mvbind (p1start p1end) (find-phrase phrase1 vdoc :match :words)
      (mvbind (p2start p2end) (find-phrase phrase2 vdoc :match :words :ignore-start p1start :ignore-end p1end)
	(when (or (null p1start) (null p2start))        ;; a phrase was not found
	  (return-from extract-order-and-features (values nil nil)))
	(let ((ordered (if (< p1start p2start) t nil)))
	  (let ((before (max 0 (1- (if ordered p1start p2start))))
		(between-start (1+ (if ordered p1end p2end)))
		(between-end (1- (if ordered p2start p1start)))
		(after (1+ (if ordered p2end p1end)))
		(features nil))
	    ;; Get features
	    (labels ((getfv (offset location)
			    (get-template-feature-vector-id 
			     feature-space 
			     (get-token-id vdoc offset)
			     (get-tag vdoc offset)
			     location)))
	      (loop for offset from 0 upto before do
		   (push (getfv offset :before) features))
	      (loop for offset from between-start upto between-end do
		    (push (getfv offset :between) features))
	      (loop for offset from after upto (1- (length (document-text vdoc))) do
		   (push (getfv offset :after) features)))
	    (values features ordered)))))))

(defun extract-gross-features (doc feature-space) 
  (values 
   (loop for offset from 0 upto (1- (length (document-text doc))) do
	 (get-template-feature-vector-id
	  feature-space
	  (get-token-id doc offset)
	  (get-tag doc offset)
	  :between))
   :fwd))
	   

(defun extract-phrase-pairs-and-features (vdoc feature-space)
  "Given a sentence as a vector document, find all noun and verb phrases, generate the various
   possible pairings of them, generate feature vectors for each pair for later classification
   and analysis.  Returns a list of tuples (phrase-tuple, feature-list)"
  (declare (ignore vdoc feature-space)))
    
;;
;; Top level function that pulls all relations from conceptnet and trains
;; according to the trainer function
;;


;; ------------------------------------------------
;; Advanced feature space classifier training
;; ------------------------------------------------

(defclass cnet-classifier (naive-bayes)
  ((feature-space :accessor cnet-classifier-feature-space :initform (make-template-feature-space))
   (class-space :accessor cnet-classifier-class-space :initform (make-cnet-class-space))))

(defun make-cnet-classifier (&key index-fn (hint 1000))
  (make-instance 'cnet-classifier
		 :vector-hint hint
		 :index-fn (if index-fn index-fn (make-hashed-index-fn hint))))

(defmethod predict-class ((classifier cnet-classifier) features &rest opts)
  "Handles unpacking the class vector.  Should this procedure also pack 
   feature lists?"
  (declare (ignore features opts))
  (let ((class (call-next-method)))
    (dbind (pred direction) (get-cnet-class-vector classifier class)
	   (values pred direction))))

(defmethod get-template-feature-vector-id ((classifier cnet-classifier) token pos location)
  (get-template-feature-vector-id (cnet-classifier-feature-space classifier) token pos location))
(defmethod get-template-feature-vector ((classifier cnet-classifier) id)
  (get-template-feature-vector (cnet-classifier-feature-space classifier) id))
(defmethod get-cnet-class-vector-id ((classifier cnet-classifier) predicate direction)
  (get-cnet-class-vector-id (cnet-classifier-class-space classifier) predicate direction))
(defmethod get-cnet-class-vector ((classifier cnet-classifier) id)
  (get-cnet-class-vector (cnet-classifier-class-space classifier) id))

(defmethod match-concepts-to-phrases (sentence concepts)
  (let ((pairs (append 
		(get-annotation sentence :event-chunks)
		(get-annotation sentence :np-chunks))))
    (mapcar (lambda (concept)
	      (car (find concept pairs :key #'cdr)))
	    concepts)))

;; (defmethod train-cnet-classifier-over-omcs ((classifier cnet-classifier) sentence-index &key max)
;;   "Trains classifier over sentences and relations in idhash for easy analysis"
;;   (let ((count 0))
;;     (map-relations
;;      (lambda (pred head tail)
;;        (when (and max (>= count max))
;; 	 (return-from train-cnet-classifier-over-omcs t))
;;        (when (= 0 (mod (incf count) 1000))
;; 	 (format t "~A~%" count))
;;        (train-cnet-classifier-on-relation classifier sentence-index pred head tail)))))

;; (defmethod train-cnet-classifier-on-relation ((classifier cnet-classifier) 
;; 					      sentence-index predicate head tail)
;;   (awhen (find-sentences-from-concepts sentence-index head tail)
;;     (loop for sentence in it do
;;        (destructuring-bind (hphrase tphrase)
;; 	   (match-concepts-to-phrases sentence (list head tail))
;; ;;	 (format t "head ~A tail ~A~%" hphrase tphrase)
;; 	 (when (and hphrase tphrase)
;; 	   (multiple-value-bind (features forward)
;; 	       (extract-order-and-features sentence hphrase tphrase 
;; 					   (cnet-classifier-feature-space classifier))
;; 	     (when features
;; 	       (format t "Training on: ~A for (~A ~A ~A)~%" 
;; 		       (vector-document-string sentence :with-tags t :with-newline t)
;; 		       (get-type-name predicate) 
;; 		       (mapcar #'token-for-id (phrase-words hphrase))
;; 		       (mapcar #'token-for-id (phrase-words tphrase)))
;; 	       (let ((cvecid (get-cnet-class-vector-id classifier predicate (if forward :fwd :bck))))
;; 		 (train-classifier classifier features cvecid)))))))))

(defun get-simple-features (space sentence)
  (mapcar (lambda (word)
	    (get-template-feature-vector-id space word :ANY :ALL))
	  (array->list sentence)))

(defmethod train-relation-classifier ((classifier cnet-classifier) sentence predicate)
  (let ((features (get-simple-features (cnet-classifier-feature-space classifier)
				       (if (stringp sentence)
					   (string->token-array sentence)
					   sentence))))
    (when features
;;      (format t "Training sentence...~%")
      (let ((cvecid (get-cnet-class-vector-id classifier predicate :fwd)))
	(train-classifier classifier features cvecid)))))

(defmethod predict-relation ((classifier cnet-classifier) sentence)
  (let* ((features (get-simple-features (cnet-classifier-feature-space classifier)
					sentence))
	 (type (predict-class classifier features)))
    (get-type-name type)))
	 

(let ((types nil))
  (defun eval-filter (type)
    (not (member type 
		 (aif-ret types
			  (setf types
				(mapcar #'conceptnet::get-type-id 
					'("isa" "capableof" "effectof" "locationof" "capableofreceivingaction"
					  "motivationof" "desireof" "propertyof" "usedfor" 
					  "subeventof" "partof" "desirouseffectof"))))))))

;; -------------------
;; OMCS Evaluation
;; -------------------

;; Take a subset of the total relations (as generated by Hugo's tools)
;; and determine if we characterize the same relation for the same sentence

(defun compare-relations (reference-file idhash classifier)
  "Look at a predicate in the reference file, look up its source sentence
   in the idhash then apply the open-text classifier to the sentence and 
   compare to the predicate type and ordering of the original predicate"
  (declare (ignore reference-file idhash classifier)))
  
  

;;
;; Interactive functions
;; 

(defun predict-sentence-class (classifier sentence-string)
  (conceptnet::get-type-name 
   (predict-class classifier (get-lemmatized-sentence sentence-string))))

;; (defun try-train-relation-remainder (classifier index pred-name sent1 sent2)
;;   (let ((s1 (get-lemmatized-sentence sent1))
;; 	(s2 (get-lemmatized-sentence sent2))
;; 	(pred-id (conceptnet::get-type-id pred-name)))
;;     (train-relation-remainder classifier index pred-id s1 s2)))

;; To capture runs at the REPL

(defun do-setup (&optional (limit 500000))
  (declare (special index omcs idhash))
  (setf idhash (make-hash-table :size 300000))
  (setf index (make-instance 'index))
  (setf omcs (load-omcs-for-study "~/Work/think/trunk/data/lang/en/omcs/omcsraw_id.txt" index idhash :max-lines limit))
  t)

(defun do-learn (&optional (limit nil))
  (declare (special classifier index))
  (setf classifier (make-cnet-classifier))
  ;; Train over conceptnet relations
  (train-cnet-classifier-over-omcs classifier index :max limit))

(defun do-eval ()
  (declare (special classifier index idhash))
  ;; loop over predicates, find sentences and see if we 
  ;; reproduce the predicate
  (maphash (lambda (k v) (print k v)) idhash))


;; ----------------------------------------
;; Unit test of advanced classifier
;; ----------------------------------------

(defparameter *ref-set*
  '(("A dog is a kind of mammal" "dog" "mammal" "isa")
    ("A frog is a kind of amphibian" "frog" "amphibian" "isa")
    ("frogs are amphibians" "frogs" "amphibians" "isa")
    ("dogs are mammals" "frogs" "amphibians" "isa")
    ("Murderers are capable of killing" "murderers" "killing" "capableof")
    ("Dogs are capable of barking" "dogs" "barking" "capableof")))

(defparameter *test-set*
  '(("A boat is a kind of transportation device" "boat" "transportation device" "isa")
    ("Dogs are mammals" "dogs" "mammals" "isa")
    ("Frogs are capable of jumping" "frogs" "jumping" "capableof")))

(defstruct ref-entry doc head tail pred)

(defun build-phrase (string)
  (make-phrase (string->token-array string) nil))

(defun parse-ref (list)
  (make-ref-entry
   :doc (vector-tag-tokenized (string-downcase (first list)))
   :head (build-phrase (second list))
   :tail (build-phrase (third list))
   :pred (get-type-id (fourth list))))

(defun test-cnet-classifier ()
  (let ((refs (mapcar #'parse-ref *ref-set*))
	(tests (mapcar #'parse-ref *test-set*))
	(classifier (make-cnet-classifier))
	(index (make-instance 'index)))
    (loop for ref in refs do
	  (update-index index (ref-entry-doc ref) (array->list (document-text (ref-entry-doc ref)))))
    (loop for ref in refs do
	  (multiple-value-bind (features direction)
	      (extract-order-and-features (ref-entry-doc ref) (ref-entry-head ref) (ref-entry-tail ref)
					  (cnet-classifier-feature-space classifier))
	    (let ((cvecid (get-cnet-class-vector-id classifier (ref-entry-pred ref) (if direction :fwd :bck))))
	      (train-classifier classifier features cvecid))))
    (loop for ref in tests do
	  (predict-document-relation classifier (ref-entry-doc ref) (ref-entry-head ref) (ref-entry-tail ref)))))

(defun predict-document-relation (classifier doc head tail &optional prediction)
  (multiple-value-bind (features direction)
      (extract-order-and-features doc head tail (cnet-classifier-feature-space classifier))
    (declare (ignore direction))
    ;; If head and tail didn't match, just assume all are between
    (when (not features)
      (mvbind (f d) (extract-gross-features doc (cnet-classifier-feature-space classifier))
	(setf features f)
	(setf direction d)))
    ;; Return prediction
    (if prediction
	(format t "predicted: ~A  actual: ~A~%" (get-type-name prediction) (get-type-name (predict-class classifier features)))
      (format t "guessed relation: ~A~%" (get-type-name (predict-class classifier features))))))

