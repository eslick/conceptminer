;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: eventminer -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          queries.lisp
;;;; Purpose:       Simple inverse index queries over elephant's inverse index hack
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  February 2006
;;;;

(in-package :conceptminer)

;;
;; Single inverse lookups
;;

(defmethod map-pages (fn (idx inverse-index) (token fixnum))
  (with-miner-store ()
    (let ((token-idx (get-value token (inverse-index-root idx))))
      (when token-idx
	(with-btree-cursor (cur token-idx)
	  (loop while (cursor-next cur) do
	       (multiple-value-bind (valid? page set) (cursor-current cur)
		 (declare (ignore set valid?))
		 (funcall fn page))))))))
		 
(defmethod find-pages-for-token ((idx inverse-index) token &aux pages)
  (map-pages (lambda (page) (push page pages)) idx token)
  pages)

;;
;; A query interpreter for finding proximate sequences
;;		 

(defmethod query-inverse-index ((idx inverse-index) query &key (near-range 20) (lemma-query t))
  "(tok tok * tok NEAR tok tok) => returns (cons page range-list)
   range-list = (cons start-loc end-loc) for each matched expression"
  (let* ((query (typecase query (string (parse-query-string query)) (list query)))
	 (parsed-query (parse-query-expr idx (prepare-query idx query :lemma lemma-query) nil near-range)))
    (if (or (eq parsed-query :not-found)
	    (member :not-found parsed-query)
	    (wildcard-query? parsed-query))
	(clean-open-cursors parsed-query)
	(let ((acur (make-instance 'and-cursor :cursor-list (sorted-cursor-list parsed-query)))
	      (page-phrases nil))
	  (unwind-protect
	       (loop 
		  (multiple-value-bind (valid? page) (cursor-next acur)
		    (if (not valid?)
			(return-from query-inverse-index page-phrases)
			(awhen (find-matching-queries parsed-query)
			  (if (eq (type-of page) 'page)
			      (push (cons page (make-range-phrases page it))
				    page-phrases)
			      (push page page-phrases))))))
	    (cursor-close acur))))))

(defun wildcard-query? (parsed-query)
  (let ((npos (position 'near parsed-query)))
    (and npos
	 (or (wildcard-phrase? (subseq parsed-query 0 npos))
	     (wildcard-phrase? (subseq parsed-query (1+ npos)))))))

(defun wildcard-phrase? (parsed-subquery)
  (every (lambda (tok)
	   (member tok '(:near :* * near)))
	 parsed-subquery))

(defmethod parse-query-string (query)
  (butlast (extract-words (mvretn 3 (tokenize-string (concatenate 'string query " EOF"))))))

(defun clean-open-cursors (expr)
  (when (listp expr)
    (mapcar (lambda (stmt)
	      (when (eq (type-of stmt) 'query-cursor)
		(cursor-close (query-cursor-db-cursor stmt))))
	    expr)))
		      
(defmethod prepare-query ((idx inverse-index) query &key (lemma nil))
  (unless (null query)
  (dbind (elt . rest) query
    (typecase elt
      (string 
       (prepare-query idx (cons (get-lemma-for-id (id-for-token elt)) rest)))
      (symbol
       (cons elt (prepare-query idx rest)))
      (fixnum 
       (cond ((eq elt (id-for-token "*"))
	      (cons '* (prepare-query idx rest)))
	     ((eq elt (id-for-token "near"))
	      (cons 'near (prepare-query idx rest)))
	     ((and (inverse-index-remove-stopwords-p idx) (stopword? elt))
	      (cons '* (prepare-query idx rest)))
	     ((inverse-index-lemmatize-p idx)
	      (cons (get-lemma-for-id elt) 
		    (prepare-query idx rest)))
	     (t (cons elt (prepare-query idx rest)))))))))

(defun make-range-phrases (page pairs)
  (mapcar #'(lambda (pair)
	      (make-phrase-from-vdoc (page-contents page)
				     (car pair) 
				     (- (cdr pair) (car pair) (- 1))
				     :query-range))
	  pairs))

(defun sorted-cursor-list (query)
  (sort (copy-list query) #'< :key #'query-cursor-size))

(defun parse-query-expr (idx query last near-range)
  (cond ((null query)
	 (when last
	   (setf (query-cursor-range last) nil)))
	((consp query)
	 (typecase (car query)
	   (fixnum (let ((token-index (get-value (car query) idx)))
		     (if (not token-index)
			 (cons :not-found nil)
			 (let ((qcur (make-query-cursor
				      :db-cursor (make-cursor token-index)
				      :size (token-index-size token-index)
				      :range 1)))
			   (cons qcur 
				 (parse-query-expr idx (cdr query) qcur near-range))))))
	   (symbol
	    (when last
	      (case (car query)
		('* (incf (query-cursor-range last)))
		('near (incf (query-cursor-range last) (1- near-range)))
		(t (error "Unrecognized query term ~A in query expression" (car query) query))))
	    (parse-query-expr idx (cdr query) last near-range))
	   (t (error "Unrecognized element ~A in query parsing" (car query)))))
	(t (error "Parse error in query parsing: ~A" query))))

(defun find-matching-queries (pquery &aux sets)
  "Searches for NEAR/AND matches and returns a
   list of start/end intervals"
  (mapc #'update-query-cursor-array pquery)
  (with-slots (array offset range) (car pquery)
;;    (print array)
    (loop for start across array do 
	   (if (cdr pquery)
	       (awhen (find-next-range (cdr pquery) (1+ start) (+ start range))
		 (push (cons start it) sets))
	       (push (cons start start) sets)))
    sets))

(defun find-next-range (pquery base bound)
  (with-slots (array offset range) (car pquery)
      (loop 
	 (when (or (not (arrayp array)) (>= offset (length array)))
	   (return nil))
	 (let ((loc (aref array offset)))
	   (cond ((or (null loc) (not (integerp loc)))
		  (error "Non integer value in array ~A" array))
		 ((< loc base)
		  (incf offset))
		 ((and (>= loc base) (<= loc bound))
		  (incf offset)
		  (if (null (cdr pquery))
		      (return loc)
		      (return (find-next-range (cdr pquery) (1+ loc) (+ loc range)))))
		 ((> loc bound)
		  (return nil)))))))

;; Think about query in principled manner:
;; - wildcards (range 2)
;; - range (within N)
;; - phrases (sequential sets)
;; - compound (phrase then phrase)
;; - boolean (AND / NOT) (ignore OR for now)

;; token = token-id | '* 
;; phrase = char+ | ( compound | phrase | range )
;; range = phrase NEAR phrase
;; compound = phrase [AND | OR] phrase 


(defun make-and-cursor-from-query (idx query)
  (let ((cursors (collect (lambda (token)
			    (when (eq (type-of token) 'fixnum)
			      (let ((tidx (get-value token idx)))
				(when tidx
				  (make-query-cursor :db-cursor (make-cursor tidx)
						     :size (token-index-size tidx)
						     :range 1)))))
			  (prepare-query idx query))))
    (values (make-instance 'and-cursor :cursor-list cursors)
	    cursors)))
		     

(defun explore-btree (idx &key (break nil) (print t) (query t) (key-print nil) (value-print nil))
  (flet ((key-printer (key) (if key-print (funcall key-print key) key))
	 (value-printer (value) (if value-print (funcall value-print value) value)))
  (with-btree-cursor (cur idx)
    (loop while (cursor-next cur) do
	 (multiple-value-bind (valid? key value) (cursor-current cur)
	   (declare (ignore valid?))
	   (when print (format t "key: ~A  value: ~A~%" (key-printer key) (value-printer value)))
	   (when break (break))
	   (when query 
	     (when (equal (read-line) "q")
	       (return))))))))


;;
;; TEST ROUTINES
;;

(defun make-page-from-file (url query filename)
  (make-page url query :contents (vector-tag (read-file-to-string filename))))

(defun make-page-from-doc (url query doc)
  (make-page url query :contents doc))

(defun setup-test ()
  (let ((idx (get-from-miner-root :miner-inverse-index)))
    (index-document idx (make-page-from-file "http://test1.com/" "simple query" "~/test.txt"))
    (index-document idx (make-page-from-file "http://test2.com/" "simple query" "~/test2.txt"))))

(defun stress-test-file (filename count)
  (let ((idx (get-from-miner-root :miner-inverse-index)))
    (loop for i from 1 upto count do
	 (index-document idx (make-page-from-file "http://test1.com/" "simple query" filename)))))

(defun stress-test-doc (doc count)
  (let ((idx (get-from-miner-root :miner-inverse-index)))
    (loop for i from 1 upto count do
	 (index-document idx (make-page-from-doc "http://test1.com/" "simple query" doc)))))
