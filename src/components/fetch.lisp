;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: eventminer -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          fetch.lisp
;;;; Purpose:       Persistent record of outgoing queries
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  February 2006
;;;;

(in-package :conceptminer)

;;
;; Worker processes independant of component framework 
;;    (avoids having to implement dynamic pcomp support)
;;
		   
(defun make-worker-rec (queue proc) (list queue proc t))
(defun worker-queue (entry) (first entry))
(defun worker-process (entry) (second entry))
(defun worker-busy (entry) (third entry))

(defun set-worker-process (entry value)
  (setf (second entry) value))
(defsetf worker-process set-worker-process)

(defun set-worker-busy (entry value)
  (setf (third entry) value))
(defsetf worker-busy set-worker-busy)

(defun worker-quiescent-p (rec)
  (and (queue-empty-p (worker-queue rec))
       (not (worker-busy rec))))

;;
;; A worker to fetch URLs.  Accepts 'numprocs' as a parameter
;; to establish the number of worker processes doing url fetches
;;

(defcomponent fetch-url
  (:vars (pool nil) (next-worker 0) numprocs (error-data nil))
  (:initialize (setf pool (make-fetcher-workers numprocs self)))
  (:terminate (setf pool nil))
  (:worker-error
   (push data error-data))
  (:body 
   (labels ((get-worker-queue ()
	      ;; Round robin - should even out over time
	      (when (>= (incf next-worker) numprocs)
		(setf next-worker 0))
	      (worker-queue (nth next-worker pool))))
     (when error-data
       (error "Workers encountered error: ~A" error-data))
     (enqueue (get-worker-queue) data))))

(defun worker-parent-terminated-p (parent)
  (eq (component-state parent) :terminated))

(defun worker-parent-running-p (parent)
  (eq (component-state parent) :running))

(defun worker-loop (record parent *standard-output*)
  (declare (special *standard-output*))
  (let ((queue (worker-queue record)))
    (loop until (worker-parent-terminated-p parent) do
	 (setf (worker-busy record) nil)
	 (port:process-wait nil #'(lambda () 
				    (or (and (worker-parent-running-p parent) (not (queue-empty-p queue)))
					(worker-parent-terminated-p parent))))
	 (when (and (worker-parent-terminated-p parent) 
		    (queue-empty-p queue))
	   (return))
	 (setf (worker-busy record) t)
	 (force-output *standard-output*)
	 (let ((page (dequeue queue)))
	   (when page
	     (catch 'transaction
	       (handler-case 
		   (let ((url (page-url page)))
		     (if (and (slot-boundp page 'contents)
			      (or (equal (type-of (page-contents page)) 'string)
				  (equal (type-of (page-contents page)) 'vector-document)))
			 (send-data-msg parent (cons page nil))
			 (send-data-msg parent (cons page (fetch-stripped-html url)))))
		 (error () 
		   (send-ctrl-msg parent (pcomp::name parent) 'worker-error page)))))))))

;; Takes URLs and returns stripped text strings
(defun make-fetcher-workers (count parent-comp &aux processes)
  "Returns a list of queue/process pairs that fetch 
   and return document strings to the target of the provided
   component.  It's a bit of a hack, but I didn't want to rewrite
   the framework to handle the wider set of cases."
  (dotimes (num count)
    (let* ((queue (make-instance 'list-queue))
	   (record (make-worker-rec queue nil)))
      (push record processes)
      (setf (worker-process record)
	    (port:make-process (format nil "fetcher worker ~A" num)
			       #'(lambda (r p so)
				   (worker-loop r p so))
			       record parent-comp *standard-output*))))
  processes)

(defun fetch-stripped-html (url)
  (ignore-non-text
   (html-translate-specials
    (fetch-html-page url) t)))

(defun ignore-non-text (text)
  (if (or (equal "%PDF-1.2" (subseq text 0 8))
	  (and (> (length text) 1)
	       (not (alphanumericp (aref text 0)))))
      ""
      text))

(defun fetch-html-page (url)
  "Simple fetch of an html-page given a URL"
  (format t "Fetching ~A~%" url)
  (net.aserve.client:do-http-request url :accept "text/html, text/text"))

;;; The following material has the following copyright:
;;;
;;; Copyright (C) 1997-2002 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html

(defmacro defcustom (name type init doc)
  "Define a typed global variable."
  `(progn (declaim (type ,type ,name))
    (defvar ,name (the ,type ,init) ,doc)))

(defcustom *html-specials* list
  '(("gt" . #\>) ("lt" . #\<) ("quot" . #\") ("amp" . #\&) ("nbsp" . #\Space)
    ("acute" . #\') ("ast" . #\*) ("colon" . #\:) ("comma" . #\,)
    ("commat" . #\@) ("copy" . "(C)") ("curren" . #\$) ("divide" . #\/)
    ("dollar" . #\$) ("equals" . #\=) ("excl" . #\!) ("grave" . #\`)
    ("half" . "1/2") ("hyphen" . #\-) ("lowbar" . #\_) ("lpar" . #\()
    ("rpar" . #\)) ("lsqb" . #\[) ("rsqb" . #\]) ("num" . #\#) ("period" . #\.)
    ("plus" . #\+) ("plusmn" . "+-") ("pound" . #\#) ("quest" . #\?)
    ("laquo" . "<<") ("raquo" . ">>") ("lcub" . #\{) ("rcub" . #\})
    ("semi" . #\;) ("shy" . #\-) ("times" . #\*) ("verbar" . #\|))
  "Alist of translations of HTML specials like `&*'.")

(deftype index-t () '(unsigned-byte 28))

(defun is-char-return (c)
  (declare (character c) (optimize (speed 3) (safety 0)))
  (or (char= c #\Return)
      (char= c #\Linefeed)))

(defun is-char-simple-whitespace (c)
  (declare (character c) (optimize (speed 3) (safety 0)))
  (or (char= c #\Space) (char= c #\Tab)
      (= (char-code c) 13)
      #+allegro (char= c #\%space)))

(defun collapse-whitespace2 (s)
  "Convert multiple whitespace characters to a single space character."
  (declare (simple-string s)
	   (optimize (speed 3) (safety 0)))
  (with-output-to-string (stream (make-string (length s)))
    (do ((pos 0 (1+ pos))
	 (in-white nil)
	 (in-return nil)
	 (len (length s)))
	((= pos len))
      (declare (fixnum pos len))
      (let ((c (schar s pos)))
	(declare (character c))
	(cond
	 ((is-char-simple-whitespace c)
	  (unless (or in-white in-return)
	    (write-char #\space stream))
	  (setq in-white t))
	 ((is-char-return c)
	  (unless in-return
	    (write-char c stream))
	  (setf in-return t))
	 (t
	  (setq in-white nil
		in-return nil)
	  (write-char c stream)))))))

(defun html-translate-specials (str &optional space)
  "Replace (non-destructively) HTML specals with their interpretations.
HTML tags, surrounded by `<>', are removed or replaced with a space, if
optional argument SPACE is non-nil."
  (declare (string str))
  (with-output-to-string (stream)
  (let (in-white in-return)
  (labels ((write-it (c)
	     (cond ((is-char-simple-whitespace c)
		    (unless (or in-white in-return)
		      (write-char #\space stream))
		    (setq in-white t))
		   ((is-char-return c)
		    (unless in-return
		      (write-char c stream))
		    (setf in-return t))
		   (t
		    (setq in-white nil
			  in-return nil)
		    (write-char c stream)))))
  (do ((beg 0 (1+ beg)) (len (length str)))
      ((>= beg len) nil)
    (declare (type index-t beg len))
    (case (char str beg)
      (#\< (if (ignore-start-tag-p str beg)
	       (setq beg (or (ignore-end-tag str beg) len))
	       (setq beg (or (position #\> str :start beg) len)))
	   (when space (write-it #\Space)))
      (#\&
       (let ((pa (assoc str *html-specials* :test
                        (lambda (str tag)
                          (let ((end (+ beg (length tag))))
                            (and (>= len end)
                                 (string= str tag :start1 beg
                                          :end1 end)))))))
         (cond (pa (incf beg (1- (length (car pa))))
                   (write-it (cdr pa)))
               (t (when space (write-it #\Space))
                  (setq beg (or (position #\; str :start beg) len))))))
      (t (write-it (char str beg)))))))))

(defparameter *ignore-list*
  '(("<script " 8 "</script>" 9) 
    ("<style " 7 "</style>" 8)))

(defparameter *last-ignored* nil)

(defun ignore-start-tag-p (str start)
  (declare (string str))
  (dolist (tagrec *ignore-list*)
    (when (equalp (first tagrec) 
		  (subseq str start (min (1- (length str)) (+ start (second tagrec)))))
      (setf *last-ignored* (cddr tagrec))
      (return-from ignore-start-tag-p t))))

(defun ignore-end-tag-p (str start)
  (declare (string str))
  (equalp (first *last-ignored*) 
	  (subseq str start (min (1- (length str)) (+ start (second *last-ignored*))))))

(defun ignore-end-tag (str start)
  (loop until (>= start (length str)) do
       (when (ignore-end-tag-p str start)
	 (return-from ignore-end-tag (values (+ start 8) t)))
       (setf start (position #\< str :start (1+ start))))
  (values (length str) nil))
		 
