;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: eventminer -*-

(in-package :conceptminer)


(defclass altavista-searcher (document-searcher)
  ((type :accessor searcher-content-type :initarg :content-type :initform "web")
   (cookies :accessor searcher-cookie-jar :initarg :cookie-jar 
	    :initform (make-instance 'net.aserve.client:cookie-jar))))

(defmethod search-for-proximate-phrases ((s altavista-searcher) phrase1 phrase2 distance &key &allow-other-keys)
  (declare (ignore distance))
  (do-search s 
    (concatenate 'string
		 (thttp:escape-url-query (concatenate 'string "\"" phrase1 "\""))
		 "+NEAR+"
		 (thttp:escape-url-query (concatenate 'string "\"" phrase2 "\"")))))

(defmacro with-valid-search-result ((str searcher query &key (accept-code 200)) &body body)
  (with-gensyms (text code)
    `(multiple-value-bind (,text ,code) (get-search-results ,searcher ,query)
       (when (= ,code ,accept-code)
	 (let ((,str ,text))
	   ,@body)))))

(defmethod do-search ((s altavista-searcher) query &key &allow-other-keys)
  "Takes a query string and returns a list of URLs"
  (with-valid-search-result (data s query)
    (extract-av-urls data)))

;; NOTE: Fix get-search-results to not target wikipedia!  :)

(defmethod get-search-results ((s altavista-searcher) query &optional (domain ""))
  (let ((url (format nil "http://www.altavista.com/web/results?itag=ody&pg=aq&aqa=&aqp=&aqo=&aqn=&aqmode=b&aqb=~A&kgs=1&kls=0&dt=tmperiod&d2=0&dfr%5Bd%5D=1&dfr%5Bm%5D=1&dfr%5By%5D=1980&dto%5Bd%5D=2&dto%5Bm%5D=6&dto%5By%5D=2006&filetype=&rc=dmn&swd=~A&lh=&nbq=~A"
		     query domain (searcher-max-refs s))))
    (net.aserve.client:do-http-request
	url
      :accept "text/html,text/plain"
      :user-agent "Mozilla/5.0 (Macintosh; U; PPC Mac OS X Mach-O; en-US; rv:1.8.0.1) Gecko/20060111 Firefox/1.5.0.1"
      :cookies (searcher-cookie-jar s)
      :keep-alive nil)))

(defun extract-short-urls (str &aux results)
  (cl-ppcre:do-register-groups (url)
      ("<span class=ngrn>([^<]*) </span>" str (nreverse results))
    (push url results)))

(defun extract-long-urls (str &aux results)
  (cl-ppcre:do-register-groups (url)
      ("click%3fu=http%3a//([^']*)'>" str (nreverse results))
    (push url results)))

(defun prepend-http (str)
  (concatenate 'string "http://" str))

(defun extract-av-urls (str)
  (mapcar #'prepend-http
	  (append (extract-long-urls str)
		  (filter-if (lambda (str)
			       (cl-ppcre:scan "\\.{3}" str))
			     (extract-short-urls str)))))
