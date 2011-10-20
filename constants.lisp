(in-package :harlie)

(defparameter *constants* (merge-pathnames  "SourceCode/lisp/harlie/constants/"
					    (make-pathname :directory
							   (pathname-directory
							    (user-homedir-pathname)))))

;;; country codes

(defun constant-file (fname)
  "take the filename of a file in the constants directory and return
   it. Fail with nil if it doesn't exist."
  (let ((fname (merge-pathnames fname *constants*)))
    (if (cl-fad:file-exists-p fname)
	fname
	nil)))

(defun country-codes ()
  "return a hash-table keyed by the country codes established by the
   international telecomunications union."
  (let ((ccode-hashtable (make-hash-table :test #'equal :synchronized t)))
    ;; ITU encodes this file as latin-1, which cacks in a utf-8 decode.
    (with-open-file (s (constant-file "country_codes")
		       :direction :input
		       :external-format :ISO-8859-1)
      (loop for cline = (read-line s nil nil)
	    :while cline
	    ;; :do (format t "~&|~A" cline)
	    :do (let ((ccode (strip-spaces (split-sequence #\: cline))))
		  ;; (format t "~&~A" ccode)
		  (setf (gethash (parse-integer (car ccode) :junk-allowed t)
				 ccode-hashtable) (cadr ccode)))
	    :finally (return ccode-hashtable)))))

(defparameter *country-codes* (country-codes)
  "an alist of the ITU specified country codes.")

(defun by-code (key base)
  (assoc key base :test :equalp))

(defun by-word (word)
  (let ((results nil))
    ))

(defun country-lookup (key)
  (let ((acode (parse-integer key :junk-allowed t)))
    ))

;;; /country codes
