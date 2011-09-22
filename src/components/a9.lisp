;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: eventminer -*-

(in-package :conceptminer)


(defclass a9-searcher (document-searcher)
  ((type :accessor searcher-content-type :initarg :content-type :initform "web")
   (cookies :accessor searcher-cookie-jar :initarg :cookie-jar 
	    :initform (make-instance 'net.aserve.client:cookie-jar))))

(defmethod search-for-proximate-phrases ((s a9-searcher) phrase1 phrase2 distance &key &allow-other-keys)
  (assert (< distance 10))
  (do-search s 
    (apply #'concatenate 'string
	   "\"" phrase1 "\" "
	   (append (repeat "*" distance)
		   (list "\"" phrase2 "\" ")))))

(defmethod do-search ((s a9-searcher) query &key &allow-other-keys)
  (search-a9-web s query (searcher-max-refs s)))

;; ==================
;; A9 URL Searching
;; ==================

(defmethod get-a9-response ((s a9-searcher) query &key (type "web") (page 1))
  (assert (and (integerp page) (>= page 1)))
  (let ((url (concatenate 'string "http://a9.com/" 
			       (thttp:escape-url-query (princ-to-string query))
			       (when (not (= page 1))
;;				 (format nil "?a=o~A" type)
				 (format nil "?pw=~A" page)))))
    (print url)
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

(defmacro with-a9-string ((svar searcher query &key (type "web") (page 1)) &body body)
  (with-gensyms (text code)
    `(multiple-value-bind (,text ,code) (get-a9-response ,searcher ,query :type ,type :page ,page)
       (when (= ,code 200)
	 (let ((,svar ,text))
	   ,@body)))))

(defparameter *url-extractor* (cl-ppcre:create-scanner "<a .* a9c=\"web\" href=\"([^\"]*)\".*>"))

(defun extract-a9-urls (str &aux results)
  (cl-ppcre:do-register-groups (url) (*url-extractor* str (nreverse results))
    (push url results)))

(defun extract-a9-page-count (str)
  (multiple-value-bind (match count) 
      (cl-ppcre:scan-to-strings "of about ([0-9,^\ ]*)</span>" str :sharedp t)
    (cond 
      ((not match) 0)
      ((>= (length count) 1)
       (read-from-string (string-remove-characters (aref count 0) '(#\,)))))))

(defmethod search-a9-web ((a9 a9-searcher) query count &aux urls)
  (with-a9-string (str a9 query :page 1 :type "web")
    (setf urls (append urls (extract-a9-urls str)))
    (let ((total (extract-a9-page-count str)))
      (when (> total 10)
	(loop 
	   for page from 2 upto (ceiling (/ (min count total) 10)) do
	     (with-a9-string (str a9 query :page page)
	       (setf urls (append urls (extract-a9-urls str)))
;;	       (format t "have ~A urls~%" (length urls))
	       ))))
    urls))

(defmethod get-hit-count ((s a9-searcher) query-string)
  (with-a9-string (str s query-string :page 1 :type "web")
    (extract-a9-page-count str)))
  
