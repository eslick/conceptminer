;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: conceptminer -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          window-classifier.lisp
;;;; Purpose:       Turn training windows into classifiers
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  June 2006
;;;;

(in-package :conceptminer)

(defparameter *training-window->features* nil
  "Take a training-window and return a list of features.
   This makes it easy to try a bunch of different features")

(defun train-and-test-classifier (pos-list neg-list &key (train-percent 75))
  (multiple-value-bind (pos-training-set pos-test-set) (random-subset pos-list (/ train-percent 100))
    (multiple-value-bind (neg-training-set neg-test-set) (random-subset neg-list (/ train-percent 100))
      (let ((c (make-naive-bayes-classifier)))
	(train-binary-classifier c pos-training-set t)
	(train-binary-classifier c neg-training-set nil)
	(list (acons :type :class (test-classifier c pos-test-set t))
	      (acons :type :not-class (test-classifier c neg-test-set nil)))))))

(defun train-binary-classifier (classifier training-set class-present?)
  (dolist (window training-set)
    (train-classifier classifier 
		      (funcall *training-window->features* window)
		      (if class-present? :class :not-class))))

(defun test-binary-classifier (classifier test-set class-expected?)
  (let ((total 0)
	(pass 0)
	(fail 0))
    (dolist (window test-set)
      (incf total)
      (let ((prediction (predict-class classifier (funcall *training-window->features* window))))
	(cond ((or (and (eq prediction :class) class-expected?)
		   (and (eq prediction :not-class) (not class-expected?)))
	       (incf pass))
	      (t (incf fail)))))
    `((:total . ,total) (:pass . ,pass) (:fail . ,fail))))

;; 
;; Window -> feature generators
;;

(defun generate-top-level-features (window)
  "Generate fwd/bck and other top-level features"
  (case (query-result-window-direction (training-window-result window))
    (:forward (mklist :forward))
    (:backward (mklist :backward))
    (nil nil)))

(defun generate-token-features (window &optional location)
  "Just generate the bag of tokens"
  (if (not location)
    (phrase-words (training-window-training-phrase window))
    ;; NOTE: complete this
    nil))

(defun generate-token-pos-features (window &optional location)
  "Generate pairs of token+pos features"
  (with-slots (training-phrase source-phrase target-phrase) window
    (let ((before (

(defun generate-phrase-pattern-features (window)
  "Generate interesting phrase patterns")

