(in-package :harlie)

(defparameter *constants* (merge-pathnames  "SourceCode/lisp/harlie/constants/"
					    (make-pathname :directory
							   (pathname-directory
							    (user-homedir-pathname)))))

;;; area codes

(defun constant-file (fname)
  "take the filename of a file in the constants directory and return
   it. Fail with nil if it doesn't exist."
  (let ((fname (merge-pathnames fname *constants*)))
    (if (cl-fad:file-exists-p fname)
	fname
	nil)))

(defun area-codes ()
  "return an alist of the country codes established by the
   international telecomunications union."
  (let ((ccode-alist nil))
    ;; ITU encodes this file as latin-1, which cacks in a utf-8 decode.
    (with-open-file (s (constant-file "country_codes")
		       :direction :input
		       :external-format :ISO-8859-1)
      (loop for cline = (read-line s nil nil)
	    :while cline
	    ;; :do (format t "~&|~A" cline)
	    :do (let ((ccode (strip-spaces (split-sequence #\: cline))))
		  ;; (format t "~&~A" ccode)
		  (push (cons (parse-integer
			       (car ccode) :junk-allowed t) (cadr ccode)) ccode-alist))
	    :finally (return ccode-alist)))))

(defparameter *area-codes* (area-codes)
  "an alist of the ITU specified area codes.")

;;; /area codes
