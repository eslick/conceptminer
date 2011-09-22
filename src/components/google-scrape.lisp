;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: eventminer -*-

(in-package :conceptminer)


(defclass google-searcher (document-searcher)
  ((type :accessor searcher-content-type :initarg :content-type :initform "web")
   (cookies :accessor searcher-cookie-jar :initarg :cookie-jar 
	    :initform (make-instance 'net.aserve.client:cookie-jar))))

(defmethod search-for-pattern-instance ((s google-searcher) string)
  (do-search s
      (concatenate 'string
		   ;;(thttp:escape-url-query "allintext:")
		   "allintext:"
		   "+%22"
		   string
		   "%22")))

(defmethod search-for-pattern-phrases ((s google-searcher) phrase1 phrase2 &key &allow-other-keys)
  (do-search s
    (concatenate 'string 
		 ;;(thttp:escape-url-query "allintext:")
		 "allintext:"
		 "+%22"
		 (thttp:escape-url-query phrase1)
		 "%22+*+%22"
		 (thttp:escape-url-query phrase2)
		 "%22")))

(defmethod search-for-proximate-phrases ((s google-searcher) phrase1 phrase2 distance &key &allow-other-keys)
  (assert (< distance 10))
  (do-search s 
    (apply #'concatenate 'string
	   "\"" (thttp:escape-url-query phrase1) "\" "
	   (append (repeat "*" distance)
		   (list "\"" phrase2 "\" ")))))
  
(defmethod do-search ((s google-searcher) query &key &allow-other-keys)
  (search-google-web s query (searcher-max-refs s)))

;; ==================
;; Google URL Searching
;; ==================

(defmethod get-google-response ((s google-searcher) query &key (page 1) (num 50))
  (assert (and (integerp page) (>= page 1)))
  (let ((url (format nil "http://www.google.com/search?hl=en&q=~A&num=~D~A"
		     ;; "ie=utf-8&oe=utf-8&client=firefox-a&rls=org.mozilla:en-US:official"
		     (princ-to-string query)
		     (min num (searcher-max-refs s))
		     (if (<= page 1)
			 ""
			 (format nil "&start=~A&sa=N" (1- (* (1- page) num)))))))
    (format t "URL: ~A~%" url)
    (net.aserve.client:do-http-request 
	url 
      :accept "text/html,text/plain"
      :user-agent "Mozilla/5.0 (Macintosh; U; PPC Mac OS X Mach-O; en-US; rv:1.8.0.1) Gecko/20060111 Firefox/1.5.0.1"
      :cookies (searcher-cookie-jar s)
      :keep-alive t)))
;;      :headers :foo
;;      :cookies 

;;  (thttp:http-get (concatenate 'string "http://a9.com/" 
;;			       (thttp:escape-url-query (princ-to-string query))
;;			       (if (= page 1)
;;				   (format nil "?a=o~A" type)
;;				   (format nil "?p=~A" page)))))

(defmacro with-google-string ((svar searcher query &key (page 1) (num 50)) &body body)
  (with-gensyms (text code)
    `(multiple-value-bind (,text ,code) (get-google-response ,searcher ,query :page ,page :num ,num)
       (when (= ,code 200)
	 (let ((,svar ,text))
	   ,@body)))))

(defparameter *google-url-extractor* (cl-ppcre:create-scanner "<a class=l href=\"([^\"]*)\".*>"))

(defun extract-google-urls (str &aux results)
  (cl-ppcre:do-register-groups (url) (*google-url-extractor* str (nreverse results))
    (push url results)))

(defun extract-google-page-count (str)
  (multiple-value-bind (match count) 
      (cl-ppcre:scan-to-strings "of about <b>([^<]*)</b>" str :sharedp t)
    (declare (ignore match))
    (if count
	(values (read-from-string (string-filter "," (aref count 0))))
	(values 0))))

(defun string-filter (filt str &aux (j 0))
  (let ((new (make-string (1+ (length str)) :initial-element #\Space)))
    (loop for i from 0 upto (1- (length str)) do
      (let ((ch (aref str i)))
	(unless (member-array ch filt)
	  (setf (aref new j) ch)
	  (incf j))))
    new))

(defun member-array (elt array)
  (loop for slot across array 
     finally (return nil) do
       (when (eq elt slot) (return t))))
	

(defmethod search-google-web ((gs google-searcher) query count &optional (num 50) &aux urls)
  (with-google-string (str gs query :page 1)
    (setf urls (append urls (extract-google-urls str)))
    (let ((total (extract-google-page-count str)))
      (when (> total (min num (searcher-max-refs gs)))
	(loop 
	   for page from 2 upto (ceiling (/ (min count total) num)) do
	     (with-google-string (str gs query :page page :num num)
	       (setf urls (append urls (extract-google-urls str)))
;;	       (format t "have ~A urls~%" (length urls))
	       ))))
    urls))

(defmethod get-hit-count ((gs google-searcher) query)
  (with-google-string (str gs (thttp:escape-url-query query)  :page 1)
    (extract-google-page-count str)))