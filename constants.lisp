(in-package :harlie)

(defparameter *constants* (merge-pathnames  "SourceCode/lisp/harlie/constants/"
					    (make-pathname :directory
							   (pathname-directory
							    (user-homedir-pathname)))))

;;; constants in the form of airport and country codes, areacodes, etc.

(defun constant-file (fname)
  "take the filename of a file in the constants directory and return
   it. Fail with nil if it doesn't exist."
  (let ((fname (merge-pathnames fname *constants*)))
    (if (cl-fad:file-exists-p fname)
	fname
	nil)))

(defun constant-table (file)
  "return a hash-table built from a file of key/value pairs, one per
   line, separated by a colon.."
  (let ((ccode-hashtable (make-hash-table :test #'equalp :synchronized t)))
    ;; if external format of file is latin-1 utf8 will puke. pass
    ;; 8859-1 as a safety measure.
    (with-open-file (s (constant-file file)
		       :direction :input
		       :external-format :ISO-8859-1)
      (loop for cline = (read-line s nil nil)
	    :while cline
	    ;; :do (format t "~&|~A" cline)
	    :do (let ((ccode (strip-spaces (split-sequence #\: cline))))
		  ;; (format t "~&~A" ccode)
		  (setf (gethash (car ccode)
				 ccode-hashtable) (cadr ccode)))
	    :finally (return ccode-hashtable)))))

(defparameter *country-codes* (constant-table "country_codes")
  "a hash-table of the ITU specified country codes.")

(defparameter *airport-codes* (constant-table "airport")
  "a hash-table of the IATA airport codes for most world airports.")

(defparameter *currency-codes* (constant-table "currency-codes")
  "A hash-talbe containing most ISO currency codes on earth.")

(defun dump-constant-table (table)
  (maphash (lambda (k v)
	     (format t "~&[~A][~A]" k v)) table))

(defun by-code (key base)
  (let ((place (gethash key base)))
    (if place
	(acons key place nil)
	nil)))

(defun by-word-helper (word base)
  (let ((scanner (create-scanner word :case-insensitive-mode t)))
    (if (scan scanner base)
	t
	nil)))

(defun by-word (word source)
  (declare (ignorable word))
  (loop for key being the hash-keys of source
	for val being the hash-values of source
	:if (by-word-helper word val)
	:collect (cons key val)))

(defun country-lookup (key)
  (cond
      ((every #'alpha-char-p key) (by-word key *country-codes*))
      ((every #'alphanumericp key) (by-code key *country-codes*))
      (t "No country for code or term: ~A" key)))


(defun airport-lookup (key)
  (cond
    ((= (length key) 3) (by-code key *airport-codes*))
    ((every #'alphanumericp key) (by-word key *airport-codes*))
    (t "No airport found for your puny string: ~A" key)))

(defun currency-lookup (key)
  (cond
    ((= (length key) 3) (by-code key *currency-codes*))
    ((every #'alpha-char-p key) (by-word key *currency-codes*))
    (t (format nil "No currency ISO code found for key: ~A" key))))

