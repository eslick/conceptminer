(in-package :conceptminer)

;; List of desireof pairs (1000)
;; - 100 windows each? 
;; - 50-100MB
;; - generate new patterns
;; DesireOf queries with existing patterns
;; - dog, researcher, dictator
;; - All windows on the net

(defun generate-pattern-queries-for-relation-ids (relation-ids)
  (let ((patterns (get-patterns-for-relation-ids relation-ids)))
    
;; relation-id -> pattern
;; pattern -> query
;; 

  