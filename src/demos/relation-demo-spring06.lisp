
(in-package :conceptminer)


;; ====================
;; Relation query demo
;; ====================

(defclass search-status ()
  ((urls :accessor status.urls :initarg :urls :initform 0)
   (fetched :accessor status.fetched :initarg :urls :initform 0)
   (indexed :accessor status.indexed :initarg :indexed :initform 0)
   (done :accessor status.done :initarg :done :initform nil)
   (windows :accessor status.windows :initarg :windows :initform nil)
   (guess :accessor status.guess :initarg :guess :initform nil)
   (analyzed :accessor status.analyzed :initarg :analyzed :initform nil)
   ;; To allow for cleanup
   (query :accessor status.query :initarg :query :initform nil)
   (pages :accessor status.pages :initarg :pages :initform nil)))

(defun setup-stats (container stats)
  (mapc (lambda (target)
	  (send-ctrl-msg container target :set-status stats))
	'(url-stats fetch-stats index-stats)))

(eval-when (compile load eval)
  (export '(status.urls
	    status.fetched
	    status.indexed
	    status.done
	    status.windows
	    status.guess
	    status.analyzed
	    status.query
	    status.pages
	    search-status
	    setup-stats
	    container-is-active
	    cnet-relation-hunt-demo)))

(defcomponent update-stats 
  (:vars status mutator)
  (:set-status
   (assert (typep data 'search-status))
   (setf status data))
  (:body
   (funcall mutator status data)))

(defcomponent extractor
  (:vars (source-phrase nil) (target-phrase nil) (window-size 30))
  (:set-phrases 
   (labels ((mkphrase (ph)
	      (typecase ph
		(string (make-phrase-from-sentence ph))
		(phrase ph))))
     (setf source-phrase (mkphrase (car data)))
     (setf target-phrase (mkphrase (cdr data)))))
  (:body
   (let ((source-locations 
	  (langutils::find-phrase-locations source-phrase (page-contents data)
			:match :words :lemma t))
	 (target-locations
	  (langutils::find-phrase-locations target-phrase (page-contents data)
			:match :words :lemma t)))
     (let ((pairs (find-phrase-pairs source-locations target-locations window-size)))
       (when pairs
	 (send (cons data pairs)))))))

(defun find-phrase-pairs (srcs targs dist &aux pairs)
  "Take ordered list of intervals and find all 
   pairs within some window distance"
  (let ((lasta (first srcs))
	(lastb (first targs))
	(srclist (copy-list (rest srcs)))
	(targlist (copy-list (rest targs))))
    (loop while (not (and (null srclist) (null targlist))) do
       (let ((fwd (- (car lastb) (cdr lasta)))
	     (bck (- (car lasta) (cdr lastb))))
	 (format t "~A ~A ~A ~A ~A~%" dist fwd bck lasta lastb)
	 (when (and (> fwd 0) (<= fwd dist))
	   (format t "push fwd~%")
	   (push (cons lasta lastb) pairs))
	 (when (and (> bck 0) (<= bck dist))
	   (format t "push bck~%")
	   (push (cons lastb lasta) pairs))
	 (cond ((or (and (null srclist) (not (null targlist)))
		    (>= (caar srclist) (caar targlist)))
		(setf lastb (pop targlist)))
	       ((or (and (null targlist) (not (null srclist)))
		    (< (caar srclist) (caar targlist)))
		(setf lasta (pop srclist)))
	       (t (error "~A ~A ~A ~A" lasta targlist lastb srclist))))))
  (nreverse pairs))
		    
(defcontainer cnet-relation-hunt-demo
  (:children
   (search url-search :searcher-class 'a9-searcher :urls 50 
	   :threshold 5 :threaded t)
   (url-filter filter :filter-fn 'page-filter)
   (fetch fetch-url :numprocs 8)
   (tagger text-tagger :quanta 20 :print t)
   (indexer page-indexer)
   (delay delay :amount 0.2)
   ;; diagnose
   (print-url page-printer)
   ;; status
   (url-stats update-stats :mutator (lambda (stats data)
				      (declare (ignore data))
				      (incf (status.urls stats))))
   (fetch-stats update-stats :mutator (lambda (stats data)
					(incf (status.fetched stats))
					(push (car data) (status.pages stats))))
;;   (window-stats update-stats  :mutator (lambda (stats data)
;;					(push data (status.windows stats)))))
   (index-stats update-stats :mutator (lambda (stats data)
					(declare (ignore data))
					(incf (status.indexed stats)))))
  (:netlist
   (search -> url-filter)
   (url-filter -> fetch print-url url-stats)
   (fetch -> tagger fetch-stats)
;;   (tagger -> extractor window-stats)))
   (tagger -> indexer)
   (indexer -> index-stats pause)))

(defcontainer cnet-relation-sweep-demo
  (:children
   (search url-search :searcher-class 'a9-searcher :urls 50 
	   :threshold 5 :threaded t)
   (url-filter filter :filter-fn 'page-filter)
   (fetch fetch-url :numprocs 8)
   (tagger text-tagger :quanta 20 :print t :forward-tagged-p t)
   (pause pause)
   ;; diagnose
   (print-url page-printer)
   ;; status
   (url-stats update-stats :mutator (lambda (stats data)
				      (declare (ignore data))
				      (when stats
					(incf (status.urls stats)))))
   (fetch-stats update-stats :mutator (lambda (stats data)
					(when stats
					  (incf (status.fetched stats))
					  (push (car data) (status.pages stats)))))
   (window-stats update-stats  :mutator (lambda (stats data)
					  (when stats
					    (push data (status.windows stats))))))
  (:netlist
   (search -> url-filter)
   (url-filter -> fetch print-url url-stats)
   (fetch -> tagger fetch-stats)
;;   (tagger -> extractor window-stats)))
   (tagger -> extractor)
   (extractor -> update-page window-stats)))


(defun clear-container (container)
  (mapc #'clear-input-queue
	(pcomp::container-children container))
  (mapc #'queue-clear
	(cars (slot-value (get-child container 'fetch) 'pool)))
  t)

(defun clear-input-queue (component)
  (queue-clear (pcomp::input-queue component)))

(defun container-is-active (container)
  (flet ((has-data? (name)
	   (< 0 (queue-size (pcomp::input-queue (get-child container name)))))
	 (fetcher-has-data? (queue)
	   (< 0 (queue-size queue))))
    (or (some #'has-data? '(search fetch tagger indexer))
	(awhen (get-child container 'fetch)
	  (some #'fetcher-has-data? (cars (slot-value it 'pool)))))))
