(in-package :conceptminer)

(defun setup-demo ()
  (open-miner-object-store)
  (setf *desireof-relation-list-10%* (get-from-miner-root :desireof-relation-list-10%))
  (setf *desireof-relation-list-10%-patterns* (get-from-miner-root :desireof-relation-list-10%-patterns))
  (setf *gpatterns* (extract-general-patterns *desireof-relation-list-10%-patterns* 100))
  (get-dog-data)
  (get-researcher-data)
  (get-dictator-data)
  (get-general-desireof-patterns))

(defun get-dog-data ()
  (setf *dog-instances* (get-mined-instances-for-source-and-type "desireof" "dog")))

(defun get-researcher-data ()  
  (setf *researcher-instances* (get-mined-instances-for-source-and-type "desireof" "researcher")))

(defun get-dictator-data ()
  (setf *dictator-instances* 
	(remove-nulls (get-mined-instances-for-source-and-type "desireof" "dictator"))))

(defun get-general-desireof-patterns ()
  (mapcar #'pattern-term-list *gpatterns*))
