(in-package :conceptminer)

;;
;; Utility macros
;;

(defmacro dophrase ((phrase) &body body)
  (with-gensyms (ph)
    `(let ((,ph ,phrase))
       (loop for offset from 0 upto (1- (phrase-length ,ph)) do
	    ,@body))))

;;
;; Preprocessing phrases
;;

(defvar *filter-constraints* nil
  "Contains a list of functions that return t if phrase satisfies constraint")

(defmethod valid-training-window-p (window)
  "Filters must return t if it accepts the phrase"
  (every #'(lambda (filter-name)
	     (funcall (symbol-function filter-name) (training-window-phrase window)))
	 *filter-constraints*))

;;
;; Simple constraints
;;

(defun needs-verb (phrase)
  "A good window must contain a verb"
  (dophrase (phrase)
    (when (verb-pos? (get-tag phrase offset))
      (return-from needs-verb t)))
  nil)

(defparameter *max-noun-phrase* 4)

(defun short-noun-phrases (phrase)
  "A good window contains short noun phrases"
  (let ((count 0))
    (dophrase (phrase)
      (if (noun-pos? (get-tag phrase offset))
	  (if (> (incf count) *max-noun-phrase*)
	      (return-from short-noun-phrases nil))
	  (setf count 0)))
    t))

;;
;; Web constraints
;;

(defparameter *commercial-keywords* nil)

(defun ensure-keywords ()
  (unless *commercial-keywords*
    (mapcar #'id-for-token
	    '("categories"
	      "search"
	      "gallery"
	      "password"
	      "price"
	      "sponsored"))))

(defun filter-commercial-keywords (phrase)
  (ensure-keywords)
  (dophrase (phrase)
    (when (member (get-token-id phrase offset) *commercial-keywords*)
      (return-from filter-commercial-keywords nil))))

;;
;; Commonsense constraints
;;



;;
;; Initialize constraints
;;

(eval-when (:load-toplevel :execute)
  (when (not *filter-constraints*)

    (push 'short-noun-phrases *filter-constraints*)
    (push 'needs-verb *filter-constraints*)))

;;
;; Utilities
;;

(defun verb-pos? (x)
  (member x '(:VBD :VBP :VBG :VBN :VBZ)))

(defun noun-pos? (x)
  (member x '(:NN :NNP :NNPS :NNS)))

(defun punctuation? (x)
  (member x '(:|,| :|.| :|:| :|;| :|"| :|\\| :|'| :|\|| )))

