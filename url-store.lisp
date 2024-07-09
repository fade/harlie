;;;; url-store.lisp

(in-package #:harlie)

(defclass postmodern-url-store ()
  ((readwrite-url-db :initform (psql-botdb-credentials *bot-config*) :accessor readwrite-url-db)))

(defvar *the-url-store* (make-instance 'postmodern-url-store))

(defclass urls ()
  ((url-id :col-type integer :accessor url-id)
   (input-url :col-type string :initarg :input-url :accessor input-url)
   (redirected-url :col-type string :initarg :redirected-url :accessor redirected-url)
   (short-url :col-type string :initarg :short-url :accessor short-url)
   (title :col-type string :initarg :title :accessor title)
   (from-nick :col-type string :initarg :from-nick :accessor from-nick)
   (tstamp :col-type integer :initform (timestamp-to-unix (now)) :accessor tstamp)
   (dead :col-type boolean :initform nil :accessor url-dead-p)
   (context-id :col-type integer :initarg :context-id :accessor context-id))
  (:metaclass dao-class)
  (:keys url-id))

;;; some janitorial functions to mark urls that 404 'dead' in the
;;; database, and to try to ensure a title for each url.

(defun list-all-urls ()
  "return a list of urls dao objects, one for each url in the
   shortener db."
  (let ((dbconn (readwrite-url-db *the-url-store*)))
    (with-connection dbconn (select-dao 'urls "true order by random()"))))

(defun list-n-urls (n &key (urls (list-all-urls)))
  "take the first n urls retrieved from the urlstore. primarily to
   limit for testing other janitorial features."
  (loop for i to (1- n) ;; indexes start at 0. ask for 12, get 12.
	for url in urls
	:collect url))

(defun none-title-urls ()
  "This query will select for a condition that only happens in a
   url-shortener database that was inherited from the original bot
   code."
  (select-dao 'urls (:= 'title "None")))

(defun not-ascii-urls ()
  "This query will select for a condition that only happens in a
   url-shortener database that was inherited from the original bot
   code."  
  (select-dao 'urls (:= 'title "Can not downconvert to ascii.")))

(defun scan-urls-with-fn (fn &key (urls (list-all-urls)))
  "forex: (scan-urls-with-fn #'url-resolves-p :urls (list-n-urls 10))
   => (#<URLS {1015DBA7B1}> #<URLS {1015DBD461}>)
   (#<URLS {1015DB9BC1}> #<URLS {1015DBBC81}> #<URLS {1015DBC871}>
   #<URLS {1015DBE051}> #<URLS {1015DBEC41}> #<URLS {1015DBFCA1}>
   #<URLS {1008D19D81}> #<URLS {1008D19DA1}>)[at time of this
   writing.] ... the first value is the list of 'bad' urls. the
   second, the ones with status returns < 400."
  (declare (ignorable fn))
  (setf lparallel:*kernel* (lparallel:make-kernel *threads*))
  (let ((good (list))
        (bad (list)))
    (lparallel:pmapc #'(lambda (url)
                         (if (url-resolves-p url)
                             (push url good)
                             (push url bad))) urls)
    (lparallel:end-kernel)
    (values bad good)))

(defun url-resolves-p (urlobj)
  "the url resolves if the get status is not in the 400 range. If it
   resolves, return T, else return NIL"
  (multiple-value-bind (stream status) (webget (input-url urlobj) :want-stream t)
    (unwind-protect
	 (cond
	   ((and status (< status 400))
	    (format t "~&URL GOOD: [~A]" (input-url urlobj))
	    t)
	   (t
	    (format t "~&URL BAD: [~A]" (input-url urlobj))
	    nil))
      (when stream
        (close stream)))))

(defun bad-url-indexes (&key (urls (list-n-urls 10)))
  (let ((urls (scan-urls-with-fn #'url-resolves-p :urls urls)))
    (loop for i in urls
	  :do (format t "~&([~0,6D]~%[~A]~%[~A]~%[Dead? ~A])~%~%"
		      (url-id i) (input-url i) (title i) (url-dead-p i)))))

(defmethod set-dead ((url urls))
  (format t "dead:: ~A~%" (title url))
  (setf (url-dead-p url) t))

(defmethod reset-title ((url urls))
  (if (or (string-equal (title url) "Can not downconvert to ascii.")
	  (string-equal (title url) "None"))
      (let ((new-title (fetch-title (input-url url))))
	(if new-title
	  (progn
	    (format t "~&~%>>old: ~A~%>>new: ~A" (title url) new-title)
	    (setf (title url) new-title))
	  (format t "~&~%NO title for Old url:: ~A" (input-url url))))))

(defun url-janitor (&key (urls (list-all-urls)))
  (let ((dbconn (readwrite-url-db *the-url-store*)))
    (multiple-value-bind (badurls goodurls) (scan-urls-with-fn #'url-resolves-p :urls urls)
      (with-connection dbconn
	(loop for url in badurls :do (progn
                                       (log:debug "Setting dead: ~A" (input-url url))
				       (set-dead url)
				       (update-dao url)))
	(loop for url in goodurls :do (progn
					(log:debug "Resetting title: ~A" (title url))
                                        (reset-title url)
					(update-dao url)))))))

(defparameter *letterz* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defparameter *how-short* 5)

(defun make-shortstring ()
  "Generate a single random short-URL string."
  (apply #'concatenate 'string
	 (loop for i from 1 to *how-short* collecting
					   (string (random-elt *letterz*)))))

(defgeneric make-unique-shortstring (store url)
  (:documentation "Assign a new short URL string to URL."))

(defmethod make-unique-shortstring ((store postmodern-url-store) url)
  (with-connection (readwrite-url-db store)
    (do ((short (make-shortstring) (make-shortstring)))
	((not (query (:select '* :from 'urls :where (:= 'short-url short))))
	 short))))

(defgeneric lookup-url (store context url nick)
  (:documentation "Return present or new short URL and title for specified URL."))

(defun make-short-url-string (context hash)
  "Compose the short URL string for a given hash in a given context."
  (format nil "~A~A" (make-url-prefix (bot-web-server context) (bot-web-port context)) hash))

(defmethod lookup-url ((store postmodern-url-store) context url nick)
  (let ((result (with-connection (readwrite-url-db store)
		  (query (:order-by
			  (:select 'short-url 'title
				   :from 'urls
				   :where (:or (:= 'input-url url)
					       (:= 'redirected-url url)))
			  (:raw "tstamp desc"))))))
    (if result
	(destructuring-bind (short title) (first result)
	  (values (make-short-url-string context short) title))
	(multiple-value-bind (title message redirect-uri) (fetch-title url)
	  (setf redirect-uri (format nil "~A" redirect-uri))
	  (cond (title
		 (let ((short (make-unique-shortstring store url))
		       (tweet (twitter-payload url)))
		   (with-connection (readwrite-url-db store)
		     (when tweet (setf title (format nil "@~A ~A" (twitter-twit url) tweet)))
		     (insert-dao (make-instance 'urls
						:input-url url
						:redirected-url redirect-uri
						:short-url short
						:title title
						:from-nick nick
						:context-id (url-write-context-id context)))
		     (values (make-short-url-string context short) title))))
		(message
		 (let ((short (make-unique-shortstring store url)))
		   (with-connection (readwrite-url-db store)
		     (insert-dao (make-instance 'urls
						:input-url url
						:redirected-url redirect-uri
						:short-url short
						:title (format nil "~A: ~A" message url)
						:from-nick nick
						:context-id (url-write-context-id context)))
		     (values (make-short-url-string context short) message))))
		(t (values nil nil)))))))

(defgeneric get-url-from-shortstring (store short)
  (:documentation "Return the full URL associated with a given short string."))

(defmethod get-url-from-shortstring ((store postmodern-url-store) short)
  (with-connection (readwrite-url-db store)
    (caar (query (:select 'redirected-url :from 'urls :where (:= 'short-url short))))))

(defgeneric get-urls-and-headlines (store context)
  (:documentation "Get a list of URLs and headlines from an URL store."))

(defmethod get-urls-and-headlines ((store postmodern-url-store) (context bot-context))
  (with-connection (readwrite-url-db store)
    (query (:order-by
	    (:select 'redirected-url 'title 'from_nick 'tstamp
                     :from 'urls
                     :where (:and
                             (:= 'context-id (url-read-context-id context))
                             (:not 'dead)))
	    (:raw "tstamp desc")))))

(defparameter *suppress-url-encoding* t)

(defun fetch-formatted-url (url-string &rest args)
  "Retrieve the lhtml contents of the page at a specified URL.
Does format-style string interpolation on the url string."
  (chtml:parse
   (http-request
    (apply 'format nil url-string args))
   (chtml:make-lhtml-builder)))
