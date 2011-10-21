;;;; url-store.lisp

(in-package :harlie)

(defclass url-store () ())

(defclass hash-url-store (url-store)
  ((url->short :initform (make-hash-table :test 'equal :synchronized t) :accessor url->short)
   (short->url :initform (make-hash-table :test 'equal :synchronized t) :accessor short->url)
   (url->headline :initform (make-hash-table :test 'equal :synchronized t) :accessor url->headline)))

(defclass postmodern-url-store (url-store)
  ((readonly-url-dbs
    :initform
    '(("botdb" "semaphor" nil :unix)
      ("bootsydb" "semaphor" nil :unix)
      ("shogundb" "semaphor" nil :unix)
      ("thugdb" "semaphor" nil :unix))
    :accessor readonly-url-dbs)
   (readwrite-url-db :initform '("harliedb" "semaphor" nil :unix) :accessor readwrite-url-db)))

(defvar *the-url-store* (make-instance 'hash-url-store))

(defvar *pomo-url-store* (make-instance 'postmodern-url-store))

(defgeneric get-url-from-old-shortstring (store url)
  (:documentation "Check the existing databases for entries corresponding to a given shortstring."))

(defmethod get-url-from-old-shortstring ((store postmodern-url-store) short)
  (dolist (db (readonly-url-dbs store))
    (with-connection db
      (let ((long
	      (query (sql (:select 'url :from 'urls :where (:= 'shorturl short))))))
        (when long (return (caar long)))))))

(defparameter *letterz* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defparameter *how-short* 5)

(defun make-shortstring ()
  "Generate a single random short-URL string."
  (apply #'concatenate 'string
	 (loop for i from 1 to *how-short* collecting
					   (string (elt *letterz* (random (length *letterz*)))))))

(defgeneric make-unique-shortstring (store url)
  (:documentation "Assign a new short URL string to URL."))

(defmethod make-unique-shortstring ((store hash-url-store) url)
  (sb-ext:with-locked-hash-table ((short->url store))
    (sb-ext:with-locked-hash-table ((url->short store))
      (do ((short (make-shortstring) (make-shortstring)))
	  ((not (gethash short (short->url store)))
	   (progn
	     (setf (gethash short (short->url store)) url)
	     (setf (gethash url (url->short store)) short)
	     short))))))

(defgeneric lookup-url (store url)
  (:documentation "Return present or new short URL and title for specified URL."))

(defmethod lookup-url ((store hash-url-store) url)
  (let ((short (sb-ext:with-locked-hash-table ((url->short store))
		 (gethash url (url->short store)))))
    (if short
	(sb-ext:with-locked-hash-table ((url->headline store))
	  (list short (gethash url (url->headline store))))
	(let ((title (fetch-title url)))
	  (if title
	      (progn
		(setf short (make-unique-shortstring store url))
		(sb-ext:with-locked-hash-table ((url->headline store))
		  (setf (gethash url (url->headline store)) title))
		(list short title))
	      (list nil nil))))))

(defgeneric get-url-from-shortstring (store short)
  (:documentation "Return the full URL associated with a given short string."))

(defmethod get-url-from-shortstring ((store hash-url-store) short)
  (sb-ext:with-locked-hash-table ((short->url store))
    (gethash short (short->url store))))

(defgeneric get-urls-and-headlines (store)
  (:documentation "Get a list of URLs and headlines from an URL store."))

(defmethod get-urls-and-headlines ((store hash-url-store))
  (sb-ext:with-locked-hash-table ((url->headline store))
    (loop for k being the hash-keys in (url->headline store)
	  collecting
	  (list k (gethash k (url->headline store) "Click here for a random link.")))))

(defun fetch-formatted-url (url-string &rest args)
  "Retrieve the lhtml contents of the page at a specified URL.
Does format-style string interpolation on the url string."
  (chtml:parse
   (http-request
    (apply 'format nil url-string args))
   (chtml:make-lhtml-builder)))
