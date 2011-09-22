;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: eventminer -*-

(in-package :eventminer)

;; Analysis functions

(defun new-phrases (newlist oldlist &key (key #'cdr) (test #'phrase-equal))
  (remove-if 
   #'(lambda (a)
       (find a oldlist :key key :test test))
   newlist
   :key key))
  
(defun changed-scores (newlist oldlist &key (key #'cdr) (test #'phrase-equal))
  (collect #'(lambda (new)
	      (aif (find (cdr new) oldlist :key key :test test)
		   (if (neq (car it) (car new))
		       (- (car new) (car it))
		     nil)))
	   newlist))
		       

;;
;; Parsing various kinds of text files
;;

(defun read-scored-phrase (stream ra &key (score-key :score) anno-key anno-value)
  "Works with cllib read-stream-to-list.  ra should be the #'read-line function value"
  (mvbind (score offset) (read-from-string ra)
	  (let ((vdoc (vector-tag (subseq ra offset))))
	    (values
	     (lemmatize-phrase 
	      (make-instance 'phrase
			     :type 'classified-event
			     :document vdoc
			     :start 0
			     :end (1- (length-of vdoc))
			     :annotations `((,score-key . ,score)
					    (,anno-key . ,anno-value))))
	     (read-line stream nil port:+eof+)))))

(defun load-scored-phrases-with-annotation (file annotation-key annotation-value &key (score-key :score))
  "Load a file consisting of a score and a phrase, create a phrase representation
   and annotate it with the key/value pair for later analysis."
;;  (read-list-from-file file (gen-read-scored-phrase-with-annotation annotation-key annotation-value
;								     :score-key score-key)))
  (read-list-from-file file #'read-scored-phrase 
		       :read-ahead-function #'read-line
		       :eof port:+eof+
		       :args `(:anno-key ,annotation-key 
			       :anno-value ,annotation-value 
			       :score-key ,score-key)))

(defun batch-load-scored-phrases-with-annotation (description &key (anno-key :classification))
  "Load a set of phrases according to the description: '((file anno-value) ...)"
  (mapcan (lambda (entry)
	    (dbind (file anno-value) entry
		   (load-scored-phrases-with-annotation file anno-key anno-value)))
	  description))

;;
;; Comparison of hand ranking to automatic scoring
;;

(defun evaluate-scoring-functions (functions classifed-phrases ranked-phrases))
  
(defun read-annotated-text-phrases (file))
  

;;
;; Plots
;;
    