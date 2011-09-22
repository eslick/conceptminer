(in-package :conceptminer)

(defpclass experiment ()
  ((name :accessor experiment-name :initarg :name :index t)
   (type :accessor experiment-type :initarg :type)
   (feature-gen :accessor experiment-feature-generator :initarg :fgen)
   (input :accessor experiment-input :initarg :input)
   (output :accessor experiment-output :initarg :output)
   (results :accessor experiment-results :initarg :results :initform nil)
   (date :accessor experiment-date :initarg :date :index t :initform (get-universal-time))))

(defun experiment-classifier-stats (experiment name)
  (let ((result (assoc name (experiment-results experiment))))
    (when (not result) (error "No results for classifier run for ~A found." name))
    (dbind (class . not-class) (cdr result)
      (assert (eq (second (first class)) :class))
      (assert (eq (second (first not-class)) :not-class))
      (format t "~A:~%" name)
      (format t "Precision: ~A~%" 
	      (compute-experiment-precision class not-class))
      (format t "Recall: ~A~%"
	      (compute-experiment-recall class)))))

(defun compute-experiment-precision (class not-class)
  (flet ((gv (rec label)
	   (cdr (assoc label rec))))
    (coerce (/ (gv class :pass)
	       (+ (gv class :pass) (gv not-class :fail)))
	    'double-float)))

(defun compute-experiment-recall (class)
  (flet ((gv (rec label)
	   (cdr (assoc label rec))))
    (coerce (/ (gv class :pass)
	       (gv class :total))
	    'double-float)))

;;
;; Datasets
;;

(defpclass dataset ()
  ((name :accessor dataset-name :initarg :name :index t)
   (type :accessor relation-type :initarg :relation-type)
   (training-data :accessor training-data :initarg :training-data)
   (testing-data :accessor testing-data :initarg :testing-data)))


(defparameter *test-set-size* .3)

(defun make-dataset (name training-windows)
  (mvbind (train test) (random-subset training-windows *test-set-size*)
    (make-instance 'dataset
		   :name name
		   :type (training-window-relation-type (first training-windows))
		   :training-data train
		   :testing-data test)))

(defun make-negative-dataset (dataset)
  (flet ((negative-name (prefix name)
	   (etypecase name
	     (string (format nil "~A-~A" prefix name))
	     (symbol (intern-format "~A-~A" prefix name)))))
    (make-instance 'dataset
		   :name (negative-name "random-windows-from" (dataset-name dataset))
		   :type (negative-name "negative-samples-for" (relation-type dataset))
		   :training-data (generate-random-negative-samples (training-data dataset))
		   :testing-data (generate-random-negative-samples (training-data dataset)))))

;;(defun generate-random-negative-samples (windows)
;;  (loop for window in windows collecting
;;       (let* ((page (training-window-page window))
;;	      (interval (training-window-interval window))
;;	      (rstart (random (length-of page))))
;;	 (make-training-window (


(defun dump-dataset (dataset filename feature-gen width)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (loop for window in (append (training-data dataset) (testing-data dataset)) do
	 (let ((features (funcall feature-gen window)))
	   (format stream "~{~A ~}~{~A ~}~%" features (repeat 0 (- width (length features)))))))
  t)

(defun dump-binary-distribution-for-matlab (filename classifier dataset feature-gen)
  (let* ((feature-map (hash-items (naive-bayes-feature-dict classifier)))
	 (features (length feature-map)))
    (flet ((find-index (token)
	     (let ((entry (find token feature-map :key #'cdr)))
	       (when entry
		 (car entry)))))
      (with-open-file (stream filename :direction :output :if-exists :supersede)
	(loop for window in (append (training-data dataset) (testing-data dataset)) do
	     (let ((features (funcall feature-gen window))
		   (feature-vector (make-array features :element-type 'fixnum :initial-element 0)))
	       (loop for feature in features do
		    (awhen (find-index feature)
		      (incf (aref feature-vector it))))
	       (format stream "~{~A ~}~%" (array->list feature-vector))))))))

;;
;; Top Level validation
;;

(defun validate-binary-classifiers (name type feature-generator-name datasets &aux classifiers reports)
  "Run an experiment on binary classifiers"
  (dotimes (i (length datasets))
    (let* ((classifier (make-naive-bayes-classifier))
	   (positive-dataset (nth i datasets))
	   (negative-datasets (remove positive-dataset datasets))
	   (feature-generator (symbol-function feature-generator-name)))
      (push (cons (relation-type positive-dataset) classifier) classifiers)
      (train-with-dataset classifier feature-generator positive-dataset negative-datasets)
      (push (cons 
	     (relation-type positive-dataset)
	     (test-with-dataset classifier feature-generator
				positive-dataset negative-datasets))
	    reports)
      (format t "Results of \"~A\" dataset:~%~A~%" 
	      (dataset-name positive-dataset)
	      (car reports))))
  (list (make-instance 'experiment 
		       :name name
			 :type type
			 :fgen feature-generator-name
			 :input datasets
;;		 :output classifiers
			 :results reports)
	classifiers))

(defun compute-accuracy (pos-pass pos-fail neg-pass neg-fail)
  (let* ((total (+ pos-pass pos-fail neg-pass neg-fail))
         (noise (/ neg-fail (+ pos-pass neg-fail)))
	 (precision (/ pos-pass (+ pos-pass pos-fail))))
    (values `(:precision ,(coerce precision 'float))
	    `(:noise ,(coerce noise 'float)))))


;;
;; Training
;;
	
(defun train-with-dataset (classifier fgen pos negs)
  "Train a classifier with training windows"
  (format t "Training positive samples with \"~A\" dataset~%" (dataset-name pos))
  (train-binary-classifier classifier fgen (training-data pos) t)
  (format t "Training negative samples with \"~A\" datasets~%" (mapcar #'dataset-name negs))
  (train-binary-classifier classifier fgen 
			   (equal-sized-test-set (mappend #'training-data (mklist negs)) 
						 (training-data pos))
			  nil))

(defun train-binary-classifier (classifier fgen training-set class-present?)
  (dolist (sample training-set)
    (when (valid-training-window-p sample)
      (let ((features (funcall fgen sample)))
	(if features
	    (train-classifier classifier features (if class-present? :class :not-class))
	    (warn "Sample ~A has no features according to ~A." sample fgen))))))

;;
;; Testing
;;

(defun test-with-dataset (classifier fgen pos negs)
  "Test a classifier with training windows"
  (format t "Testing positive results with \"~A\" dataset~%" (dataset-name pos))
  (format t "Testing negative results with \"~A\" datasets~%" (mapcar #'dataset-name negs))
  (cons (test-binary-classifier classifier fgen (testing-data pos) t)
	(test-binary-classifier classifier fgen 
				(equal-sized-test-set (mappend #'testing-data (mklist negs)) (testing-data pos))
				nil)))

(defun test-binary-classifier (classifier fgen test-set class-expected?)
  (let ((total 0)
	(pass 0)
	(fail 0))
    (dolist (sample test-set)
      (when (valid-training-window-p sample)
	(let ((features (funcall fgen sample)))
	  (if features
	      (progn
		(incf total)
		(mvbind (prediction probabilities)
		    (predict-class classifier features)
		  (declare (ignorable probabilities))
		  (cond ((or (and (eq prediction :class) class-expected?)
			     (and (eq prediction :not-class) (not class-expected?)))
			 (incf pass))
			(t (incf fail)))))
	      (warn "Sample ~A has no features according to ~A." sample fgen)))))
    `((:expected ,(if class-expected? :class :not-class))
      (:total . ,total) (:pass . ,pass) (:fail . ,fail))))

(defun equal-sized-test-set (gen ref)
  (let ((g (length gen))
	(r (length ref)))
    (if (> g r)
	(random-subset gen (/ r g))
	gen)))





