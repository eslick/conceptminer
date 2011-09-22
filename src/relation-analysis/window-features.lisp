(in-package :conceptminer)

;;
;; Features
;;

(defun window-contains-location-p (location window)
  (declare (optimize (speed 3) (space 0) (safety 1)))
  (and (>= location (interval-start window))
       (<= location (interval-end window))))

(defun positions-but (window &rest excludes &aux positions)
  (assert (legal-interval-p window))
  (mapa-b (lambda (offset)
	    (unless (some (curry #'window-contains-location-p offset) excludes)
	      (push offset positions)))
	  (interval-start window)
	  (interval-end window))
  (nreverse positions))

(defmethod generate-features ((window training-window) compute-feature)
  ;; list of words that aren't phrases
  (when (fucked-up-window-p window) ;; some window spans are backwards
    (compute-window-extent window))
  (unless (fucked-up-window-p window) ;; some are still screwed up, not sure why
    (let ((plist (positions-but (training-window-interval window)
				(training-window-source-interval window)
				(training-window-target-interval window)))
	  (vdoc (page-contents (training-window-page window))))
      (remove-nulls (mapcar (curry compute-feature vdoc) plist)))))

(defmethod window-region-intervals ((window training-window))
  (with-slots (source-interval target-interval window-interval) window
    (let ((first nil)
	  (second nil))
      (if (eq (training-window-direction window) :forward)
	  (progn
	    (setf first source-interval)
	    (setf second target-interval))
	  (progn
	    (setf first target-interval)
	    (setf second source-interval)))
      (values
       (make-interval (interval-start window-interval) (1- (interval-start first)))
       first
       (make-interval (1+ (interval-end first)) (1- (interval-start second)))
       second
       (make-interval (1+ (interval-end second)) (interval-end window-interval))))))

;;
;; Specific feature generator top-level functions
;;

;; word only features

(defun word-features (vdoc offset)
  "Generate a unique token for every word"
  (let ((id (get-token-id vdoc offset)))
    (assert (eq (type-of id) 'fixnum ))
    id))

(defun generate-word-features (window)
  (generate-features window #'word-features))

(defun lemma-features (vdoc offset)
  "Generate a unique token for every word"
  (let ((id (get-token-id vdoc offset)))
    (assert (eq (type-of id) 'fixnum ))
    (get-lemma id)))

(defun generate-lemma-features (window)
  (generate-features window #'lemma-features))

;; word+pos features

(defmethod word+pos-features (vdoc offset)
  (assert (document-tags vdoc))
  (unless (or (not (arrayp (document-text vdoc)))
	      (> offset (length-of vdoc)))
    (let ((token (get-token-id vdoc offset))
	  (tag (get-tag vdoc offset)))
      (assert (eq (type-of token) 'fixnum))
      (assert (eq (type-of tag) 'symbol))
      (cons token tag))))

(defmethod generate-word-pos-features ((window training-window))
  ;; list of word+pos for non-phrase elements
  (generate-features window #'word+pos-features))

;; word+location features

;;(defun generate-word-loc-features ()
;;  (let ((src (training-window-source-interval 

;;(defun generate-word-pos+loc-features ()
;;  )

;; words+pos and templates

(defun generate-template-features ()
  ;; word/pos+pos in order
  )

(defun generate-modifier-features ()
  ;; special support for negatives, etc
  )
