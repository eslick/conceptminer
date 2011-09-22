(in-package :conceptminer)

;; System that takes a document, a classifier and finds suspected instances 
;; of that classifier in the document

;; Find all concepts
;; Find all pairs
;; Extract windows for each pair
;; evaluate window
;; return successful windows w/ constitutents

;;
;; Main API
;;

(defun discover-relations (page classifiers feature-generator &key (width 20) (margin 10) (force nil))
  (awhen (and (not force)
	      (get-instances-by-value 'extraction-window 'page page))
    (return-from discover-relations it))
  (let* ((windows (find-extraction-windows page width margin)))
    (remove-nulls 
     (loop for window in windows collecting
	  (progn
	    (let ((matches (collect #'(lambda (assoc)
					(when (predict-binary-class (cdr assoc) :class :not-class 
								    (funcall feature-generator window))
					  (car assoc)))
				    classifiers)))
	      (setf (window-classification window) matches)
	      (if matches 
		  window
		  nil)))))))

;;
;; Extraction windows
;;

(defpclass extraction-window ()
  ((page :accessor extraction-window-page :initarg :page :index t)
   (first-interval :accessor extraction-window-first-interval :initarg :first-interval)
   (second-interval :accessor extraction-window-second-interval :initarg :second-interval)
   (window-interval :accessor window-interval :initarg :window-interval)
   (classification :accessor window-classification :initarg :classification :initform nil)
   (width :accessor training-window-width :initarg :width :initform 20)
   (margin :accessor training-window-margin :initarg :margin :initform 10)))

(defun make-extraction-window (page fi si width margin)
  (awhen (get-instances-by-value 'extraction-window 'page page)
    (let ((offset (position fi it :key #'extraction-window-first-interval
			    :test #'equal)))
      (when offset (return-from make-extraction-window (nth offset it)))))
  (make-instance 'extraction-window
		 :page page
		 :first-interval fi
		 :second-interval si
		 :width width
		 :margin margin))

(defmethod print-extraction-window ((win extraction-window))
  (print-phrase (make-phrase-from-vdoc (page-contents (extraction-window-page win))
				       (car (window-interval win))
				       (- (cdr (window-interval win)) (car (window-interval win))))
		:with-tags nil))
		
(defmethod generate-features ((window extraction-window) compute-feature)
  (let ((plist (positions-but (window-interval window)
			      (extraction-window-first-interval window) 
			      (extraction-window-second-interval window)))
	(vdoc (page-contents (extraction-window-page window))))
    (mapcar (curry compute-feature vdoc) plist)))

(defmethod compute-window-extent ((win extraction-window) &optional new-margin)
  (with-slots (page first-interval second-interval width margin) win
    (when new-margin (setf margin new-margin))
    (let ((max-extent (length-of (page-contents page))))
      (setf (window-interval win)
	    (cons (-clamp (interval-start first-interval) margin 0)
		  (+clamp (interval-end second-interval) margin (1- max-extent)))))
    win))

(defun review-extraction-windows (wins)
  (loop for window in wins do
       (print (window-classification window))
       (princ "\"")
       (print-interval (page-contents (extraction-window-page window)) (extraction-window-first-interval window))
       (princ "\" \"")
       (print-interval (page-contents (extraction-window-page window)) (extraction-window-second-interval window))
       (format t "\"~%")
       (print-extraction-window window)
       (princ #\Newline)))

(defun print-interval (vdoc interval)
  (print-phrase (make-phrase-from-vdoc vdoc 
				       (car interval)
				       (1+ (- (cdr interval) (car interval))))
		:with-tags nil :newline nil))

;;
;; Find windows
;;

(defun find-extraction-windows (page width margin)
  (let* ((vx+event-phrases (sort (append (get-event-chunks (page-contents page))
				      (get-vx-chunks (page-contents page)))
			      #'start-order)))
    (let ((pairs 
	   (merge-overlapping-phrases 
	    (get-phrase-pairs-for-list vx+event-phrases width)
	    width)))
      (mapcar #'(lambda (pair)
		  (dbind (first . second) pair
		    (let ((first-interval (cons (phrase-start first) (phrase-end first)))
			  (second-interval (cons (phrase-start second) (phrase-end second))))
		      (compute-window-extent 
		       (make-extraction-window page
					       first-interval
					       second-interval
					       width
					       margin)))))
	      pairs))))

(defun get-phrase-pairs-for-list (phrases width)
  (labels ((pairer (previous rest pairs)
	     (if (null rest)
		 (nreverse pairs)
		 (let ((current (car rest)))
		   (if (and (not (bad-phrase-p current))
			    (not (overlapping-phrase-p previous current))
			    (> width (- (phrase-start current) (phrase-end previous))))		       (pairer current (cdr rest) (cons (cons previous current) pairs))
		       (pairer current (cdr rest) pairs))))))
    (when (cdr phrases)
      (pairer (car phrases) (cdr phrases) nil))))

(defun bad-phrase-p (p1)
  (> (phrase-length p1) 5))

(defun overlapping-phrase-p (p1 p2)
  (flet ((inside-phrase (loc phrase)
	   (and (>= loc (phrase-start phrase))
		(<= loc (phrase-end phrase)))))
    (or (inside-phrase (phrase-end p1) p2)
	(inside-phrase (phrase-start p1) p2))))

(defun merge-overlapping-phrases (pairs width)
  (labels ((merge-overlaps (prior pairs result)
	     (if (null pairs)
		 (nreverse result)
		 (let ((current (car pairs)))
		   (if (and (> width (- (phrase-end (cdr current)) (phrase-start (car prior))))
			    (overlapping-phrase-p (cdr prior) (car current)))
		       (let ((new (cons (car prior) (cdr current))))
			 (merge-overlaps new (cdr pairs) result))
		       (merge-overlaps current (cdr pairs) (cons prior result)))))))
    (merge-overlaps (car pairs) (cdr pairs) nil)))


;; UTILS

(defun start-order (p1 p2)
  (< (phrase-start p1) (phrase-start p2)))
