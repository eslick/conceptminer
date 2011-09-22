(in-package :conceptminer)

(defun build-histogram-table-for-type (type)
  (let ((concepts (get-instances-by-class 'pconcept))
	(hash (make-hash-table)))
    (dolist (concept concepts)
      (let ((rids (get-relation-ids-for-concept-by-type-id concept (get-ptype type))))
	(pushnew concept (gethash (length rids) hash))))
    hash))

(defun print-relations-from-table (count type histogram-table)
  (loop for pair in (get-relation-ids-from-table count type histogram-table) do
       (format t "~A~%" (car pair))
       (mapcar #'(lambda (rid) (print-relation (get-relation rid)))
	       (cdr pair))))

(defun get-relation-ids-from-table (count type histogram-table)
  "Returns a list of pairs; concept & relation-ids for a given hash
   value in histogram table"
  (mapcar #'(lambda (concept)
	      (cons concept 
		    (get-relation-ids-for-concept-by-type-id 
		     concept 
		     (get-ptype type))))
	  (gethash count histogram-table)))

(defun select-relations (rids &key (slime t) &aux yes no)
  (loop for rid in rids do
       (print-relation (get-relation rid))
       (if (case (read-char) 
	     (#\y t)
	     (#\q (return-from select-relations (values yes no))))
	   (push rid yes)
	   (push rid no))
       (when slime (read-char)))
  (values yes no))

;;
;; Top Desireof Concepts:
;; 

(defparameter *top-desireof-concepts* 
  '("person" "cat" "dog" "invester" "painter" "researcher" "musician" "dictator"
    "criminal" "employee" "forester" "communist" "father" "shopper" "gambler" 
    "baker" "judge" "lover" "pilot" "student" "animal"))

(defun select-relations-from-concepts (concepts type &key (slime t))
  (multiple-value-bind (yes no)
      (select-relations (get-relation-ids-for-concepts concepts type) :slime slime)
    (values yes no)))

(defun get-relation-ids-for-concepts (concepts type)
  (mapcan #'(lambda (concept)
	      (get-relation-ids-for-concept-by-type-id concept (get-ptype type)))
	  (remove-nulls (mapcar #'find-pconcept concepts))))