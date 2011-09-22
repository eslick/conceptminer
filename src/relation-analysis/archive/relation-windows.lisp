(in-package :conceptminer)

;; =============
;; Analysis
;; =============

;; In root: :effectof-relation-list
;;          :desireof-relation-list
(defun get-training-windows (relations &key (range 30) &aux sentences)
  (loop for relation-rec in relations 
        for i from 0 do
        (let ((windows (windows-for-relation (cdr relation-rec) :range range)))
	  (when windows
	    (print i) (print windows))
	  (loop for window in windows do
	       (when (not (null window))
		 (push (list relation-rec
			     (first window) ;; page
			     (cons (phrase-start (second window))
				   (phrase-end (second window))))
		       sentences)))))
  sentences)

(defun windows-for-relation (rel &key (range 30))
  (query-inverse-index *conceptminer-inverse-page-index*
		       (concatenate 'list
				    (pconcept-phrase (get-pconcept (relation-source-id rel)))
				    (list 'NEAR)
				    (pconcept-phrase (get-pconcept (relation-target-id rel))))
		       :near-range range))


(defun windows->text (windows &key (margin 5))
  (loop for window in windows collecting
       (dbind (rrec page interval) window
	      (list (get-ptypename (relation-type-id (cdr rrec)))
		    (pconcept-string (get-pconcept (relation-source-id (cdr rrec))))
		    (pconcept-string (get-pconcept (relation-target-id (cdr rrec))))
		    (token-array->string
		     (subseq (document-text (page-contents page))
			     (-clamp (car interval) margin 0)
			     (+clamp (cdr interval) margin (length-of (page-contents page)))))))))

(defun -clamp (value sub floor)
  (let ((res (- value sub)))
    (if (>= res floor)
	res
	floor)))

(defun +clamp (value add ceiling)
  (let ((res (+ value add)))
    (if (<= res ceiling)
	res
	ceiling)))

(defun nth-relation (n set)
  (cdr (nth n set)))

(defun print-nth-relation (n set)
  (print-relation (nth-relation n set)))

(defun save-dataset (filename fn &rest args)
  (with-open-file (stream filename :direction :output :if-exist :supersede)
    (write (apply fn args) :stream stream)))

