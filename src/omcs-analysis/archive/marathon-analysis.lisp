;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: eventminer -*-

(in-package :eventminer)

(defparameter *barb-hand-files-distance*
  '(("~/Documents/Research/CenterForCommonSense/Mining/HandAnalysis/SecondRun1-2-05/marathon-run-distance-only-cleaned.txt" :clean)
    ("~/Documents/Research/CenterForCommonSense/Mining/HandAnalysis/SecondRun1-2-05/marathon-run-distance-only-garbage.txt" :garbage)
    ("~/Documents/Research/CenterForCommonSense/Mining/HandAnalysis/SecondRun1-2-05/marathon-run-distance-only-instance.txt" :instance)
    ("~/Documents/Research/CenterForCommonSense/Mining/HandAnalysis/SecondRun1-2-05/marathon-run-distance-only-nonsense.txt" :nonsense)
    ("~/Documents/Research/CenterForCommonSense/Mining/HandAnalysis/SecondRun1-2-05/marathon-run-distance-only-numbers.txt" :numbers)
    ("~/Documents/Research/CenterForCommonSense/Mining/HandAnalysis/SecondRun1-2-05/marathon-run-distance-only-same.txt" :same)))

(defparameter *barb-hand-files-correlation*
  '(("~/Documents/Research/CenterForCommonSense/Mining/HandAnalysis/SecondRun1-2-05/marathon-run-correlation-only-cleaned.txt" :clean)
    ("~/Documents/Research/CenterForCommonSense/Mining/HandAnalysis/SecondRun1-2-05/marathon-run-correlation-only-garbage.txt" :garbage)
    ("~/Documents/Research/CenterForCommonSense/Mining/HandAnalysis/SecondRun1-2-05/marathon-run-correlation-only-instance.txt" :instance)
    ("~/Documents/Research/CenterForCommonSense/Mining/HandAnalysis/SecondRun1-2-05/marathon-run-correlation-only-nonsense.txt" :nonsense)
    ("~/Documents/Research/CenterForCommonSense/Mining/HandAnalysis/SecondRun1-2-05/marathon-run-correlation-only-numbers.txt" :numbers)
    ("~/Documents/Research/CenterForCommonSense/Mining/HandAnalysis/SecondRun1-2-05/marathon-run-correlation-only-same.txt" :same)))

(defparameter *barb-hand-files-sum*
  '(("~/Documents/Research/CenterForCommonSense/Mining/HandAnalysis/SecondRun1-2-05/marathon-run-sum-cleaned.txt" :clean)
    ("~/Documents/Research/CenterForCommonSense/Mining/HandAnalysis/SecondRun1-2-05/marathon-run-sum-garbage.txt" :garbage)
    ("~/Documents/Research/CenterForCommonSense/Mining/HandAnalysis/SecondRun1-2-05/marathon-run-sum-instance.txt" :instance)
    ("~/Documents/Research/CenterForCommonSense/Mining/HandAnalysis/SecondRun1-2-05/marathon-run-sum-nonsense.txt" :nonsense)
    ("~/Documents/Research/CenterForCommonSense/Mining/HandAnalysis/SecondRun1-2-05/marathon-run-sum-numbers.txt" :numbers)
    ("~/Documents/Research/CenterForCommonSense/Mining/HandAnalysis/SecondRun1-2-05/marathon-run-sum-same.txt" :same)))

(defun plot-barb-distance-histogram (rec scoring &rest opts 
					 &key (normalize nil) (legend t) (num-bins nil)
					 &allow-other-keys)
  (apply #'compute-multiple-class-histogram
	 `(,(get-sorted-phrases-best-first rec scoring)
	   ,(lambda (phrase) (get-annotation phrase :classification))
	   :normalize ,normalize :legend ,legend :num-bins ,num-bins 
	   :title ,(concatenate 'string (symbol-name scoring) 
				(format nil " SCORING (Total samples: ~A)" (length (cdr (assoc scoring rec)))))
	   ,@(rem-keywords opts '(:num-bins :legend :normalize)))))

(defun get-sorted-phrases-best-first (rec key)
  (sort 
   (copy-list (cdr (assoc key rec)))
   (lambda (p1 p2)
     (> (get-annotation p1 :score)
	(get-annotation P2 :score)))))

(defun match-phrase-list (list1 list2 &key match-op (method :pairs))
  "Matches O(n^2) list1 vs. list2 and uses method to either return
   matching :pairs or to :annotate list1 objects with list2 matches
   using the :matching annotation."
  (labels ((match-phrase (phrase list)
		(if (phrase-equal phrase (car list))
		    (progn
		      (when match-op (funcall match-op phrase (car list)))
		      (car list))
		  (match-phrase phrase (cdr list)))))
    (collect #'match-phrase list1)))
    

;;(defun rank-phrases (phrases &optional (metric #'distance-scoring))
;;  "Rank phrases according to metric and leave a :score annotation")
;;  (flet ((rank-it (

;; ================================================================================================


;; --------------------------------------------------
;; Use miner to reproduce ranked phrases with pages
;; --------------------------------------------------

(defun get-raw-mined-phrases (queries &key (pages 99) (threshold 10) (scoref #'distance-scoring))
  (mine-phrases-with-google (mklist queries) 
			    :handler (make-query-phrase-return)
			    :pages pages 
			    :threshold threshold
			    :scoref scoref))

;; ---------------------------------------------
;; Pull data from Barbaras files
;; ---------------------------------------------

(defun get-barb-ranked-phrases ()
  `((:distance . ,(batch-load-scored-phrases-with-annotation *barb-hand-files-distance*))
    (:correlation . ,(batch-load-scored-phrases-with-annotation *barb-hand-files-correlation*))
    (:sum . ,(batch-load-scored-phrases-with-annotation *barb-hand-files-sum*))))

