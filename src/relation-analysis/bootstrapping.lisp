(in-package :conceptminer)

(defclass bootstrap-process ()
  ((sources :accessor bootstrap-sources :initarg :sources)
   (targets :accessor bootstrap-targets :initarg :targets)
   (seed-relations :accessor bootstrap-seed-relations :initarg :seed-relations)
   (mined-relations :accessor bootstrap-mined-relations :initform nil)
   (mined-patterns :accessor bootstrap-mined-patterns :initform nil)
   (proposed-relations :accessor bootstrap-proposed-relations :initform nil)
   (relation-type :accessor bootstrap-relation-type :initarg :relation-type)
   ;; Stats
   (iteration-count :accessor bootstrap-iteration-count :initform 0)
   (pattern-hits :accessor pattern-hit-table :initform (make-hash-table :size 2000))
   (instance-hits :accessor relation-hit-table :initform (make-hash-table :size 20000 :test #'equal))
   ))

;; pmi statistics
;; # hits for pattern by self
;; # hits for pattern with relation

;; get patterns
;; rank patterns - diversity of coverage (lots of seeds)
;; top patterns - high diversity
;; get instances with top patterns
;; rank instances by pmi with top patterns

