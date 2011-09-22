
;; Author: Ian Eslick

(in-package :conceptminer)

;;
;; Manual classification
;;

(defun review-windows (list &aux (total 0) (explicit 0) (implicit 0) (none 0) (junk 0) (skip nil))
  (loop for twin in list do
       (unless (or (training-window-judgement twin) (training-window-filtered twin))
       (if (filter-window-as-phrase (training-window-training-phrase twin))
	   (progn (princ "FILTERING:") (setf skip t) (setf (training-window-filtered twin) t))
	   (progn (incf total) (setf skip nil)))
       (print-query (query-result-window-query (training-window-result twin)))
       (print-training-window twin)
       (when (not skip)
	 (print-training-window twin :with-tags t))
       (let* ((line (read-line))
	      (ch (if (not (zerop (length line)))
		      (elt line 0)
		      #\Newline)))
	 (unless skip
	   (case ch
	     (#\e (incf explicit)  ;; explicitely expressed
		  (setf (training-window-judgement twin) :explicit))
	     (#\i (incf implicit)  ;; implicitly expressed
		  (setf (training-window-judgement twin) :implicit))
	     (#\n (incf none)      ;; nothing expressed, but good language (i.e. off topic)
		  (setf (training-window-judgement twin) :none))
	     (#\j (incf junk)      ;; crap, makes no sense, javascript, etc
		  (setf (training-window-judgement twin) :junk))))
	 (if (eq ch #\q)
	     (return (list (cons :total total) (cons :explicit explicit) 
			   (cons :implicit implicit) (cons :none none) 
			   (cons :junk junk))))))))

(defun report-training-window-stats (list &aux (total 0) (explicit 0) (implicit 0) (none 0) (junk 0))
  (dolist (twin list)
    (awhen (training-window-judgement twin)
      (incf total)
      (case it
	(:explicit (incf explicit))
	(:implicit (incf implicit))
	(:none (incf none))
	(:junk (incf junk)))))
  (list (cons :total total) (cons :explicit explicit) 
	(cons :implicit implicit) (cons :none none) 
	(cons :junk junk)))
