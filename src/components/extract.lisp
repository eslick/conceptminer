;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: eventminer -*-

(in-package :conceptminer)

;;
;; Phrase extraction and filtering
;; 

(defmethod filter-phrases-by-length-< (length phrases)
  (remove-if #'(lambda (p) (< length (phrase-length p)))
	     phrases))

;; (defmethod extract-event-phrases ((doc miner-document))
;;   (get-event-chunks (miner-doc-vector-text doc)))

;; (defmethod extract-event-phrases-cached ((doc miner-document))
;;   "Find vx-nx combos"
;;   (unless (miner-doc-events doc)
;;     (setf (miner-doc-events doc) 
;; 	  (get-event-chunks (miner-doc-vector-text doc)))
;;     (update-page-record doc))
;;   (copy-list (miner-doc-events doc)))

;; (defmethod extract-all-phrases ((doc miner-document))
;;   (setf (miner-doc-phrases doc) 
;; 	(append (get-verb-chunks (miner-doc-vector-text doc))
;; 		(get-noun-chunks (miner-doc-vector-text doc))
;; 		(get-event-chunks (miner-doc-vector-text doc))))
;;   (copy-list (miner-doc-phrases doc)))
		
;; (defmethod extract-verb-phrases ((doc miner-document))
;;   (setf (miner-doc-phrases doc) 
;; 	(get-verb-chunks (miner-doc-vector-text doc))))

;;(defmethod extract-noun-phrases ((doc miner-document))
;;  (setf (miner-doc-phrases doc) 
;;	(get-noun-chunks (miner-doc-vector-text doc))))

;;
;; Mark text sequences within the document
;; 

;; (defmethod extract-literal-phrases ((doc miner-document) literal-string &key (permissive t))
;;   "Extract instances of the provided literal from the document as a phrase list.  The
;;    permissive keyword allows us to match all surface forms of constitutent words (run, ran, running),
;;    (dog, dogs) and (marathon, Marathon, MARATHON)"
;;   (let ((matcher (compile-pattern (cons :AND (if permissive
;; 						 (mapcar #'options-for-token (split-words literal-string))
;; 						 (mapcar #'id-for-token (split-words literal-string)))))))
;;     (mapcar (curry #'make-literal-phrase 
;; 		   (miner-doc-vector-text doc))
;; 	    (find-all-patterns matcher 
;; 			       (document-text 
;; 				(miner-doc-vector-text doc))))))

(defun options-for-token (token)
  (cons :OR 
	(morph-case-surface-forms token)))

(defun make-literal-phrase (doc interval)
  (make-instance 'phrase 
		 :type :literal
		 :document doc
		 :start (car interval)
		 :end (cdr interval)))
