(in-package :conceptminer)

;; Lookup within our own database

(defclass query-db-lookup (document-searcher)
  ((record-class :accessor query-db-lookup-class :initform 'query :initarg :class)))

(defmethod do-search ((s query-db-lookup) query &key &allow-other-keys)
  (query-inverse-index *conceptminer-inverse-query-index* 
		       (query-string query)))
  

