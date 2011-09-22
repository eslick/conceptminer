(in-package :conceptminer)

;; System that takes a relation, a classifier and two concepts and attempts to determine
;; if those two concepts are part of that relation according to the classifier

(defun verify-relation-with-classifier (relation c1 c2 classifier feature-generator &key (width 30) (margin 10))
  "Verify that the positive relation trained into the binary classifier 
   between c1 and c2 (pconcepts) is valid"
  (let ((relation-ids (find-relation relation c1 c2)))
    (unless relation-ids
      (setf relation-ids (mklist (add-relation *cnet-graph* relation c1 c2 nil))))
    (evaluate-windows feature-generator classifier 
		      (mine-for-windows relation-ids width margin))))

;;
;; Evaluate to determine window types
;;

(defparameter *debug-windows* t)

(defun evaluate-windows (feature-generator classifier windows
			&aux (total 0) (yes 0) (no 0))
  "Return a yes/no on whether the classifier & feature generator
   have determined that windows illustrate the positive "
  (assert (binary-classifier-p classifier))
  (dolist (sample windows)
    (when (valid-training-window-p sample)
      (let ((features (funcall feature-generator sample))
	    (query (training-window-query sample)))
	(when features
	  (progn (incf total)
		 (mvbind (prediction score)
		     (predict-binary-class classifier :class :not-class features)
		   (if prediction (incf yes) (incf no))
		   (when *debug-windows*
		     (print-relation (get-relation (cnet-query-relation-id query)))
		     (format t "~A score ~A: ~A~%" (if prediction "MEMBER" "NOT-MEMBER") 
			     score (training-window-direction sample))
		     (print-training-window sample :stream t :with-tags t)
		     (print (sort (langutils::get-basic-chunks (page-contents (training-window-page sample)) 
							       (training-window-interval sample))
				  #'(lambda (a b) (< (phrase-start a) (phrase-start b)))))
		     (format t "~%"))))))))
  (values (cond ((< total 4)
		 :UNSURE)
		((> yes no)
		 :YES)
		(t :NO))
	  (list total yes no)))

(defun binary-classifier-p (classifier)
  ;; Verify classifier signature
  (let ((classes (naive-bayes-classes classifier)))
    (and (= 2 (length classes))
	 (member :class classes)
	 (member :not-class classes))))

;;
;; Miner wrapper to get windows
;;

(defparameter *near-window-miner-output* nil)

(defun mine-for-windows (relation-ids width margin)
  "Uses a miner component to generate windows for the provided parameters"
  (aif-ret (get-training-windows-for-relation-ids relation-ids)
    (let ((miner (get-window-miner (make-relation-recs relation-ids) width margin)))
      (unwind-protect
	   (progn
	     ;; Setup parameters for this call
	     (setf *near-window-miner-output* nil)
	     ;; (Re)Initialize
	     (execute miner)
	     ;; Wait
	     (wait-for-miner miner)
	     ;; Get results
	     (get-training-windows-for-relation-ids relation-ids))
	(release-window-miner miner)))))


(defun make-relation-recs (relation-ids)
  (mapcar (lambda (id)
	    (cons id (get-relation id)))
	  relation-ids))

;;
;; Miner wrappers to get around framework issues
;;
    
(defparameter *near-window-miner* nil)

(defun get-window-miner (relation-recs width margin)
  (unless *near-window-miner*
    (setf *near-window-miner*
	  (make-container 'cnet-relation-window-miner
			  :initrecs ;; for make-instance
			  `((search :searcher-class altavista-searcher)
			    (window-extractor :width ,width)
			    (window-extractor :margin ,margin)
			    (accum :write-to *near-window-miner-output*)
			    (rgen :list ,relation-recs)))))
  *near-window-miner*)

(defun release-window-miner (miner)
  (setf *near-window-miner* nil)
  (terminate miner)
  (sleep 0.5)
  (kill-procs-by-substring "worker")
  (kill-procs-by-substring "SEARCH"))

(defmethod miner-done-p ((miner cnet-relation-window-miner))
  (and (every #'queue-empty-p
	      (mapcar #'pcomp::input-queue
		      (pcomp::container-children miner)))
       (every #'worker-quiescent-p
	      (slot-value (get-child miner 'fetch ) 'pool))
       (eq :terminated (component-state (get-child miner 'rgen)))))

(defun wait-for-miner (miner)
  (while (not (miner-done-p miner))
    (sleep 2.0))
  (format t "Done waiting for miner~%")
  (sleep 2.0))
  
  