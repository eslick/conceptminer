
(in-package :conceptminer)

;; LITE VERSION:
;; Corrected and surface form queries
;;

(defun relation-surface-pairs (relation)
  (when relation
    (combinations (concept-surface-forms (get-pconcept (relation-source-id relation)))
		  (concept-surface-forms (get-pconcept (relation-target-id relation))))))

(defun instance-surface-pairs (source target)
  (combinations (concept-surface-forms (get-pconcept source))
		(concept-surface-forms (get-pconcept target))))

(defun concept-surface-forms (pconcept)
  "Generate concept as list of strings including the past form for
   any leading word that is a verb as well as converting person to
   useful pronoun form"
  (mapcan #'expand-verb-tenses
	  (expand-special-forms (pconcept-phrase pconcept))))

(defun expand-special-forms (phrase)
  "Convert 'something' into it and 'person' into pronouns"
  (assert (arrayp phrase))
  (nconc (list phrase)
	 (convert-something phrase)
	 (convert-person phrase)
	 (convert-person-possessive phrase)))

(defun convert-something (phrase)
  "Replaces 'something' with 'it' in phrase, returning it in a newly consed list"
  (awhen2t (replace-word "something" "it" phrase)
    (list it)))

(defun convert-person (phrase)
  (awhen (word-position "person" phrase)
    (unless (or (word-at-p (1+ it) "'" phrase)
		(word-at-p (1+ it) "'s" phrase))
      (mapcar (lambda (replacement)
		(replace-word "person" replacement phrase))
	      '("I" "me" "he" "she" "you" "we" "they" "him" "her" "us" "them")))))

(defun convert-person-possessive (phrase)
  (awhen (or (phrase-position (string->token-array "person ' s") phrase)
	     (phrase-position (string->token-array "person 's") phrase))
    (let ((new (make-array (- (length phrase) 2) :element-type 'fixnum)))
      (loop for i fixnum from 0 upto it do
	  (setf (aref new i) (aref phrase i)))
      (loop for i fixnum from (+ it 3) upto (1- (length phrase))
	    for j fixnum from (+ it 1) upto (1- (length new)) do
	  (setf (aref new j) (aref phrase i)))
      (mapcar (lambda (replacement) 
		(replace-word "person" replacement new))
	      (if (= (1+ it) (length new))
		  ;; This is the last term and so a pronoun
		  '("mine" "ours" "yours" "his" "hers" "theirs")
		  ;; This is not the last term and so a posessive adjective
		  '("my" "our" "your" "his" "her" "their"))))))

;;
;; Verb Tenses
;;

(defun expand-verb-tenses (phrase &aux forms)
  (assert (arrayp phrase))
  ;; present tense
  (push phrase forms)
  ;; past tenses
  (loop for id in (past-verb-forms (aref phrase 0)) do
       (let ((past-phrase (copy-seq phrase)))
	 (setf (aref past-phrase 0) id)
	 (push past-phrase forms)))
  forms)

(defun past-verb-forms (word-id)
  (labels ((test-past (entry)
	     (when entry
	       (member :VBD (lexicon-entry-tags entry))))
	   (try-surface-form (surface-id)
	     (if (test-past (get-lexicon-entry surface-id))
		 surface-id)))
    (let ((entry (get-lexicon-entry word-id)))
      (when entry
	(remove-duplicates 
	 (append (collect #'try-surface-form (lexicon-entry-surface-forms entry))
		 (when (test-past entry) (mklist word-id))))))))
      
;; Helper methods

(defmethod word-at (position (phrase array))
  (assert (numberp position))
  (aref phrase position))

(defmethod word-at-p (position word (phrase array))
  (assert (numberp position))
  (eq (aref phrase position) (id-for-token word)))

(defmethod word-position ((word string) (phrase array))
  (word-position (id-for-token word) phrase))

(defmethod word-position ((word fixnum) (phrase array))
  (position word phrase))

(defmethod (setf word-position) (position (word fixnum) (phrase array))
  (setf (aref phrase position) word))

(defmethod replace-word (original replacement (phrase array))
  (aif (word-position original phrase)
    (let ((new (copy-seq phrase)))
      (setf (word-position (id-for-token replacement) new) it)
      (values new t))
    (values phrase nil)))

(defmethod phrase-position ((subphrase array) (phrase array))
  (loop for offset fixnum from 0 upto (1- (length phrase)) do
       (if (loop for y fixnum from 0 upto (1- (length subphrase)) do
		(when (neq (aref subphrase y) (aref phrase (+ offset y)))
		  (return t)))
	   nil
	   (return offset))))

;; FULL VERSION:
;; Corrected & surface form queries
;;

(defun query-strings (sentence)
  (mapcar #'form->string
	  (generate-forms 
	   (extract-triples 
	    (string-downcase sentence)))))

(defstruct word-triple token pos offset)

(defun extract-triples (sentence)
  (let ((doc (vector-tag sentence))
	(triples nil))
    (loop for word from 0 upto (1- (length-of doc)) do
	 (push (make-word-triple :token (get-token-id doc word)
				 :pos (get-tag doc word)
				 :offset word)
	       triples))
    triples))

(defun generate-forms (triples)
  (mapcar (curry #'generate-form triples)
	  (surface-sets triples)))

(defun form->string (triples)
  (apply #'concatenate 'string
	 (shuffle (mapcar (lambda (triple)
			    (token-for-id 
			     (word-triple-token triple)))
			  triples)
		  (when (> (length triples) 1) (repeat " " (1- (length triples)))))))

(defun surface-sets (triples)
  (apply #'combinations 
	 (verb-surface-forms
	  (verb-triples triples))))

(defun verb-surface-forms (triples)
  (mapcar #'surface-forms-as-triples triples))

(defun surface-forms-as-triples (triple)
  (mapcar (lambda (token)
	    (make-word-triple :token  token 
			      :pos    :V 
			      :offset (word-triple-offset triple)))
	  (morph-surface-forms (word-triple-token triple) :V)))

(defun verb-triples (triples)
  (select-if #'(lambda (triple)
		 (in-pos-class? (word-triple-pos triple) :V))
	     triples))

(defun generate-form (triples surface-set)
  (sort (append (set-difference triples surface-set :key #'word-triple-offset)
		surface-set)
        #'< :key #'word-triple-offset))
   
