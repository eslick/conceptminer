(in-package :conceptminer)

;; Recover from stupid loss of mined sentences

(defvar *temp-storage* (make-hash-table :test 'equal))
;; url-hash -> (cons id url-string)

(defun load-urls (page-btree &key start stop &aux (counter 0))
  (map-btree 
   (lambda (key page)
     (declare (ignore key))
     (when (and stop (> counter stop))
       (return-from load-urls))
     (when (= 0 (mod counter 10000))
       (format t "~A~%" counter))
     (when (or (not start) (> counter start))
       (let ((url (page-url page))
	     (id (sentence-record-id (page-query page))))
	 (setf (gethash url *temp-storage*)
	       (cons id (gethash url *temp-storage*)))))
     (incf counter))
   page-btree))

(defun save-urls ()
  (maphash (lambda (url id-list)
	     (dolist (id id-list)
	       (let ((srec (get-instances-by-value 'sentence-record 'id id)))
		 (aif (get-instances-by-value 'sentence-record 'url-hash)

;; handle multiple sentences referencing the same page

