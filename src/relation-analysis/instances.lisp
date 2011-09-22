(in-package :conceptminer)

(defpclass mined-instance ()
  ((rtype :accessor relation-type :initarg :type)
   (token-pair :accessor phrase-pair :initarg :phrase-pair :index t)
   (existing :accessor existing-instance-p :initform nil :initarg :existing-p)
   (pmi :accessor mined-instance-pmi :initform nil :initarg :pmi)
   (path-length :accessor cnet-path-distance :initform nil :initarg :path-length)
   (rated :accessor mined-instance-ratings :initform nil :initarg :ratings)
   (extraction-pattern-score :accessor extraction-pattern-score :initform nil 
			     :initarg :extraction-pattern-score)
   (category :accessor instance-category :initform nil :initarg :category)))

(defpclass instance-rating ()
  ((rater :accessor rater-id :initform -1 :initarg :id)
   (garbage :accessor rating-garbage :initform nil :initarg :garbage)
   (generality :accessor rating-generality :initform nil :initarg :generality)))

;; garbage: y/n
;; generality: 0 instance 5 too general
;; 

(defmethod print-object ((i mined-instance) stream)
  (format stream "#<INSTANCE (~A ~A ~A)>"
	  (get-ptypename (relation-type i))
	  (token-array->string (phrase-pair-source (phrase-pair i)))
	  (token-array->string (phrase-pair-target (phrase-pair i)))))

(defmethod print-instance ((i mined-instance))
  (format t "(~A \"~A\" \"~A\")~%"
	  (get-ptypename (relation-type i))
	  (instance-source-string i)
	  (instance-target-string i)))

(defmethod instance-source-string ((i mined-instance))
  (conceptminer::token-array->string 
   (conceptminer::phrase-pair-source 
    (conceptminer::phrase-pair i))))

(defmethod instance-target-string ((i mined-instance))
  (conceptminer::token-array->string 
   (conceptminer::phrase-pair-target
    (conceptminer::phrase-pair i))))

	  
;;
;; Retrieve instances, mine new instances
;;

(defun make-phrase-pair (source-phrase target-phrase)
  (let ((src (typecase source-phrase
	       (phrase (phrase->token-array source-phrase))
	       (string (string->token-array source-phrase))
	       (array (if (or (adjustable-array-p source-phrase) 
			      (not (eq (array-element-type source-phrase) 'fixnum)))
			  (make-array (length source-phrase) :element-type 'fixnum :initial-contents source-phrase)
			  source-phrase))))
	(dst (typecase target-phrase
	       (phrase (phrase->token-array target-phrase))
	       (string (string->token-array target-phrase))
	       (array target-phrase))))
    (let ((lsrc (lemmatize src))
	  (ldst (lemmatize dst)))
      (list (make-array (length lsrc) :element-type 'fixnum :initial-contents lsrc)
	    (make-array (length ldst) :element-type 'fixnum :initial-contents ldst)))))

(defun phrase-pair-source (pair)
  (first pair))

(defun phrase-pair-target (pair)
  (second pair))

(defun make-mined-instance (rtype phrase-pair)
  (let ((rid (etypecase rtype
	       (fixnum rtype)
	       (string (find-ptype rtype)))))
    (aif-ret (get-mined-instance rid phrase-pair)
	     (let ((src-c (find-pconcept (phrase-pair-source phrase-pair)))
		   (dst-c (find-pconcept (phrase-pair-target phrase-pair))))
	       (make-instance 'mined-instance :type rid :phrase-pair phrase-pair
			      :existing (when (and src-c dst-c 
						   (find-relation rid src-c dst-c))))))))

;; Fix development bugs

(defun set-instances-type (type)
  (loop for inst in (get-instances-by-class 'mined-instance) do
       (setf (relation-type inst) (get-ptype type))))

(defun fix-instances-existing-bit ()
  (loop for inst in (get-instances-by-class 'mined-instance) do
       (setf (relation-type inst) (get-ptype "desireof"))
       (let ((src (find-pconcept (phrase-pair-source (phrase-pair inst))))
	     (targ (find-pconcept (phrase-pair-target (phrase-pair inst)))))
	 (when (and src targ (find-relation (relation-type inst) src targ))
	   (setf (existing-instance-p inst) t)))))

(defun fix-instance-phrase-pairs ()
  (loop for inst in (get-instances-by-class 'mined-instance) do
       (let ((src (phrase-pair-source (phrase-pair inst)))
	     (target (phrase-pair-target (phrase-pair inst))))
	 (when (or (adjustable-array-p src) (not (eq (array-element-type src) 'fixnum)))
	   (setf src (make-array (length src) :element-type 'fixnum :initial-contents src)))
	 (when (or (adjustable-array-p target) (not (eq (array-element-type target) 'fixnum)))
	   (setf target (make-array (length target) :element-type 'fixnum :initial-contents target)))
	 (let ((pair (list src target)))
	   (setf (phrase-pair inst) pair)))))

;;
;; Indexing for mined instances
;; 

(defun get-mined-instance (type-id pair)
  (car (select-if (lambda (i) (eq type-id (relation-type i)))
		  (get-instances-by-value 'mined-instance 'token-pair pair))))

(defun get-mined-instances-for-source-and-type (type source)
  (let ((all (get-instances-by-class 'mined-instance)))
    (loop for inst in all
	  when (and (or (null type)
			(and (eq (get-ptype type)
				 (relation-type inst))))
		    (equalp (as-array source)
			    (first (phrase-pair inst))))
	 collect inst)))

(defun submit-evidence (instance pattern page)
  (add-association instance (get-extraction-pattern pattern) *conceptminer-instance-to-pattern-map*)
  (add-association instance page *conceptminer-instance-to-page-map*))

(defun get-patterns-for-instance (instance)
  (get-associations instance *conceptminer-instance-to-pattern-map*))

(defun get-instances-for-pattern (pattern)
  (get-inverse-associations pattern *conceptminer-instance-to-pattern-map*))

(defun get-instances-for-query (query)
  (get-instances-for-pattern (instance-query-extraction-pattern query)))

(defun get-pages-for-instance (instance)
  (get-associations instance *conceptminer-instance-to-page-map*))

(defun get-pages-for-instance-query (query)
  (get-associations query *conceptminer-instance-query-to-page-map*))

(defun get-query-for-page (page)
  (get-inverse-associations page *conceptminer-instance-query-to-page-map*))

(defun get-instances-for-page (page)
  (get-inverse-associations page *conceptminer-instance-to-page-map*))

(defun order-instances-by-score (type)
  (sort (get-instances-with-scores type) #'> :key #'second))

(defun get-instances-with-scores (type)
  (declare (ignore type))
  (loop for instance in (get-instances-by-class 'mined-instance) collect
       (list instance (length (get-patterns-for-instance instance)))))

;;
;; Recording evidence for instances
;;

(defun record-extracted-instance (relation-type pattern page source-phrase target-phrase)
  "Integer ptype, source-phrase in any form"
  (if (or (filter-concept-string source-phrase)
	  (filter-concept-string target-phrase))
      (format t "Filtering: ~A, ~A~%" source-phrase target-phrase)
    (let ((inst (make-mined-instance relation-type (make-phrase-pair source-phrase target-phrase)))
	  (epattern (get-extraction-pattern pattern)))
      (unless (member pattern (get-patterns-for-instance inst))
	(submit-evidence inst epattern page))
      inst)))


;; TODO
(defun accept-synonym-concepts ()
  "Accept wnet synonyms for a new concept that isn't in the DB
   but has a synonym that has the same place in the same relation")
		     
(defun person-pronoun-p (id)
  (member id (mapcar #'id-for-token '("me" "you" "i" "they" "she" "he" "him" "her"))))
  
(defun as-string (phrase)
  (etypecase phrase
    (string phrase)
    (phrase (phrase->string phrase))
    (array (token-array->string phrase))))

(defun as-array (phrase)
  (etypecase phrase
    (string (string->token-array phrase))
    (array phrase)
    (phrase (phrase->token-array phrase))))

;;
;; Maintenance and research
;;

;;(defmethod review-evidence ((inst mined-instance))
;;  (let ((patterns (get-patterns-for-instance inst)))
;;    (dolist (pattern patterns)
;;      (let ((query (get-patterns-for-query
      

(defun remove-bad-instances ()
  (loop for instance in (get-instances-by-class 'mined-instance) do
       (when (or (filter-concept-string (phrase-pair-source (phrase-pair instance)))
		 (filter-concept-string (phrase-pair-target (phrase-pair instance))))
	 (drop-instances (mklist instance))
	 (drop *conceptminer-instance-to-pattern-map* instance))))

;;
;; Evaluating instances
;;	 
	 
;; For a given set of instances