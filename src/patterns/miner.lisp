(in-package :conceptminer)



;;
;; Simple phrase queries
;;

(defun get-query-phrase (query)
  (etypecase query
    (array (make-phrase query nil))
    (phrase query)
    (vector-document (make-phrase-from-vdoc query 0 (length-of query) (document-type query)))
    (query (query-phrase query))))

(defun extract-simple-windows (query page margin &aux (offset 0) (windows nil))
  (loop 
    (mvbind (start end) (find-phrase (get-query-phrase query) (page-contents page) :match :words :start offset)
      (if (null start)
	  (return)
	  (setf offset (1+ end)))
      (let* ((begin (-clamp start margin 0))
	     (length (- (+clamp end margin (length-of (page-contents page))) begin)))
	(push (make-instance 'window :query query :start begin :length length :page page)
	      windows))))
  windows)

;;
;; Extract for simple queries
;;

(defcomponent extract-windows
  (:vars (width 20) (association *conceptminer-query-to-page-map*))
  (:body 
   (let* ((page data)
	  (query (car (get-inverse-associations page association))))
     (when query
       (loop for window in (extract-simple-windows query page (ceiling width 2)) do
	    (send window))))))

;;
;; Simple interface to making miners of text windows 
;;

(defparameter *window-miner-output* nil)
(defparameter *window-miner-queries* nil)

(defun make-window-miner (&key 
			  (searcher-class 'a9-searcher) 
			  (read-from '*window-miner-queries*)
			  (write-to '*window-miner-output*)
			  (association *conceptminer-query-to-page-map*))
  (make-container 'window-miner 
		  :initrecs
		  `((search :searcher-class ,searcher-class)
		    (qgen :list ,read-from)
		    (accum :write-to ,write-to)
		    (search :association ,association)
		    (extractor :association ,association))))

;;
;; The window miner
;; 

(defcontainer window-miner
  (:children
   (qgen list-generator :list nil :rate 100 :manual nil)
   (search url-search :urls 100 :threshold 10 :threaded nil :association nil :always-associate t)
   (fetch fetch-url :numprocs 10)
   (tagger text-tagger :quanta 20 :print t)
   (extractor extract-windows :size 10 :association nil)
   (accum list-accumulator :write-to nil :print t))
  (:netlist
   (qgen -> search)
   (search -> fetch)
   (fetch -> tagger)
   (tagger -> extractor)
   (extractor -> accum)))

(defmethod miner-done-p ((miner window-miner))
  (and (every #'queue-empty-p
	      (mapcar #'pcomp::input-queue
		      (pcomp::container-children miner)))
       (every #'queue-empty-p
	      (mapcar #'worker-queue
		      (slot-value (get-child miner 'fetch ) 'pool)))
       (eq :terminated (component-state (get-child miner 'qgen)))))

(defun wait-for-miner (miner)
  (while (not (miner-done-p miner))
    (sleep 1.0)))
       

