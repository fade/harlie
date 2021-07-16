;;;; util.lisp

(in-package #:harlie)

(defun string-strip-surrounding-whitespace (str)
  "Strip whitespace characters from beginning and end of a string."
  (string-trim '(#\Space #\Newline #\Return #\Tab) str))

(defun strip-spaces (strings)
  "remove book-end whitespace from a string or list of strings."
  (cond
    ((listp strings) (mapcar #'string-strip-surrounding-whitespace strings))
    (t (string-strip-surrounding-whitespace strings))))

(defun cleanup-title (title)
  "Remove extraneous whitespace characters from within and around a string.
   Specifically: split the string on newlines; strip leading and trailing
   whitespace from each substring; drop empty substrings; compose a new string
   from the remaining substrings, with single-space separators."
  (if title
      (format nil "~{~A~^ ~}"
	      (remove-if #'(lambda (s)
			     (string= "" s))
			 (strip-spaces
			  (split-sequence #\Newline title))))
      nil))

(defun float-as-string-p (fstring)
  "Is fstring a floating point number encoded as a string?"
  (let ((scanner (create-scanner "^[0-9]*([.][0-9]*)?$")))
    (cl-ppcre:scan scanner fstring)))

(defun break-on-no-break-space (zert)
  "break up a string or list of strings by #\NO-BREAK_SPACE; these
   types of string are returned by our find-forex function as encoded
   at xe.com."
  (if (listp zert)
      (loop for string in zert
	    :collect (split-sequence:split-sequence #\NO-BREAK_SPACE string))
      (split-sequence:split-sequence #\NO-BREAK_SPACE zert)))

;;;============================================================================
;;; stock market stuff
;;;============================================================================

(defclass stock ()
  ((stock-name
    :initarg :stock-name
    :initform (error "A stock object must have a name. Please supply one.")
    :accessor stock-name)
   (stock-freshness
    :initarg :stock-freshness
    :initform nil
    :accessor stock-freshness)
   (stock-open
    :initarg :stock-open
    :initform (error "Must supply an opening price.")
    :accessor stock-open)
   (stock-high
    :initarg :stock-high
    :initform (error "Must supply a high price for the day.")
    :accessor stock-high)
   (stock-low
    :initarg :stock-low
    :initform (error "Must supply a low price for the day.")
    :accessor stock-low)
   (stock-close
    :initarg :stock-close
    :initform nil
    :accessor stock-close)
   (stock-volume
    :initarg :stock-volume
    :initform nil
    :accessor stock-volume)))

(defun jget (obj string-thing)
  "jsown is the worst name ever."
  (jsown:val obj string-thing))

(defun make-stock (name &key (function "TIME_SERIES_DAILY") (when (date-time:now)))
  (handler-case
      
      (let* ((raw-data (get-stock-values name :function function))
             (tradedays (list "Mon" "Tue" "Wed" "Thu" "Fri"))
             (rundate (simple-date-time:YYYY-MM-DD when))
             (stock-info (cond
                           ((find (simple-date-time:day-name-of rundate) tradedays)
                            (jget (jget raw-data "Time Series (Daily)") rundate))
                           (t (error "Market is closed today."))))
             (metadata (jget raw-data "Meta Data"))
             (name (jget metadata "2. Symbol"))
             (freshness (date:parse-time (jget metadata "3. Last Refreshed")))
             (open (jget stock-info "1. open"))
             (high (jget stock-info "2. high"))
             (low  (jget stock-info "3. low"))
             (close (jget stock-info "4. close"))
             (volume (jget stock-info "5. volume")))
        
        (make-instance 'stock
                       :stock-name name
                       :stock-freshness freshness
                       :stock-open (parse-number open)
                       :stock-high (parse-number high)
                       :stock-low (parse-number low)
                       :stock-close (parse-number close)
                       :stock-volume (parse-number volume)))
    (error (se)
      ;;(declare (ignorable se))
      (break)
      ;;(format t "Error: ~A~2%" se)
      )))

;; (jsown:val  (jsown:val (get-stock-values "IBM" :function "TIME_SERIES_INTRADAY") "Time Series (1min)") "2017-11-02 15:00:00")

;; (jdown:val  (jsown:val (get-stock-values "IBM") "Time Series (Daily)") (simple-date-time:YYYY-MM-DD (date-time:now)))

(defun get-stock-values (stock &key (function "TIME_SERIES_DAILY"))
  "take a stock symbol, look it up using the alphavantage.co api, and
  return a list of the values encoded in the resulting JSON object."
  (let* ((data-source (cond
                        ((string-equal function "TIME_SERIES_DAILY_ADJUSTED")
                         (format nil "~A~A~A~A~A"
                                 "https://www.alphavantage.co/query?function="
                                 function
                                 "&symbol="
                                 stock
                                 ;; "&outputsize=full"
                                 "&apikey=JMRFD5OZA2QQGJKU"))
                        ((string-equal function "TIME_SERIES_INTRADAY")
                         (format nil "~A~A~A~A~A~A"
                                 "https://www.alphavantage.co/query?function="
                                 function
                                 "&symbol="
                                 stock
                                 "&interval=1min"
                                 "&apikey=JMRFD5OZA2QQGJKU"))
                        (t
                         (format nil "~A~A~A~A~A"
                                 "https://www.alphavantage.co/query?function="
                                 function
                                 "&symbol="
                                 stock
                                 ;; "&outputsize=full"
                                 "&apikey=JMRFD5OZA2QQGJKU"))))
         (quote (strip-spaces
                 (flexi-streams:octets-to-string
                  (drakma:http-request
                   data-source)
                  :external-format :utf-8))))
    ;; (format t "~A"data-source)
    (assert quote)
    (jsown:parse quote)))

(defun make-url-prefix (server-name server-port)
  "Compose the portion of an URL encoding the server name and server port."
  (if (eql 80 server-port)
      (format nil "http://~A/"
	      server-name)
      (format nil "http://~A:~A/"
	      server-name
	      server-port)))

(defun make-pathname-in-homedir (fname)
  "Return a pathname relative to the user's home directory."
  (merge-pathnames
   fname
   (make-pathname :directory
		  (pathname-directory
		   (user-homedir-pathname)))))

(defun make-pathname-in-lisp-subdir (fname)
  "Return a pathname relative to the Lisp source code subtree in the user's home directory."
  (merge-pathnames
   fname
   (make-pathname-in-homedir "SourceCode/lisp/")))

(defun create-caseless-scanner (s)
  (create-scanner s :case-insensitive-mode t))

(defun scan-to-substrings (rx s)
  (multiple-value-bind (whole parts) (scan-to-strings rx s)
    (declare (ignore whole))
    parts))

(defun timestamp-diff (t1 t2)
  "Find the difference, in seconds, between two timestamps."
  (abs (- (timestamp-to-universal t1) (timestamp-to-universal t2))))

(defun unix-pathstring-from-pathname (pn)
  (let* ((pnt (pathname-type pn))
	 (extension (if pnt (format nil ".~A" pnt) ""))
	 (pnn (pathname-name pn))
	 (pnd (cdr (pathname-directory pn)))
	 (pc (if pnn
		 (append pnd (list pnn))
		 pnd)))
    (format nil "/~{~A~^/~}~A" pc extension)))

(defun de-utm-url (url)
  (let ((utm-index (scan "[&?][uU][tT][mM]_|[&?][mM][bB][iI][dD]" url)))
    (if utm-index
	(subseq url 0 utm-index)
	url)))
