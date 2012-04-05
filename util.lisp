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

(defun get-stock-values (stock)
  "take a stock symbol, look it up at yahoo, and return a list of the
   values returned from finance.yahoo.com: 0:stock symbol (s)
   1:last-trade-price (l1) 2:last-trade-date (d1)
   3:last-trade-time (t1) 4:change (c1) 5:open-price (o)
   6:day-high (h) 7:day-low (g) 8:volume (v)"
  (let ((quote (split-sequence #\,
			      (strip-spaces
			       (flexi-streams:octets-to-string
				(drakma:http-request
				 (format nil "~A~A~A"
					 "http://finance.yahoo.com/d/quotes.csv?s=" stock "&f=sl1d1t1c1ohgv&e=.csv"))
				:external-format :utf-8)))))
    (if quote
	(loop for i in quote :collect (remove #\" i)))))

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
