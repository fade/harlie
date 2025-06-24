(in-package #:harlie)

(defparameter *constants* (asdf:system-relative-pathname :harlie  "constants/"))

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
  (let ((ccode-hashtable (make-synchronized-hash-table :test 'equalp)))
    ;; if external format of file is latin-1 utf8 will puke. pass
    ;; 8859-1 as a safety measure.
    (with-open-file (s (constant-file file)
		       :direction :input
		       :external-format :ISO-8859-1)

      (loop for cline = (read-line s nil nil)
	    :while cline
	    :do (let* ((ccode (strip-spaces (split-sequence #\: cline)))
		       (clen (length ccode)))
		  (format t "~&|~A ~A ~A ~A" (type-of ccode) (length ccode) (car ccode) (cdr ccode))
		  (setf (gethash (string-trim "*" (car ccode)) ccode-hashtable)
			(if (= clen 2)
			    (cadr ccode)
			    (format nil "~{~A~^ ~}" (cdr ccode)))))
	    :finally (return ccode-hashtable)))))

(defun constant-read-quotes (file)
  "return a list of the quotes in FILE."
  (with-open-file (s (constant-file file)
                     :direction :input)
    (loop for cline = (read-line s nil nil)
          :while cline
          :do (format t "~&~A" cline)
              ;; (let* ((quote-candidate (strip-spaces (split-sequence #\Newline cline))))
              ;;   (format t "~&||~A " quote-candidate))
          :collect cline
          )))

(defparameter *hackers* (constant-read-quotes (constant-file "hackers"))
  "a list of quotes from the film 'Hackers'")

(defparameter *country-codes* (constant-table "country_codes")
  "a hash-table of the ITU specified country codes.")

(defparameter *area-codes* (constant-table "area-codes")
  "A fairly complete hash-table containing the telephone areacodes of
  the world.")

(defparameter *airport-codes* (constant-table "airport")
  "a hash-table of the IATA airport codes for most world airports.")

(defparameter *currency-codes* (constant-table "currency-codes")
  "A hash-talbe containing most ISO currency codes on earth.")

(defun dump-constant-table (table)
  (maphash #'(lambda (k v)
	       (format t "~&[~A][~A]" k v)) table))

(defun by-code (key base)
  (let ((place (gethash key base)))
    (format t "~&In #'by-code")
    (if place
	(acons key place nil)
	nil)))

(defun by-word-helper (word base)
  (let ((scanner (create-scanner (format nil ".*~A.*" word) :case-insensitive-mode t)))
    ;; (format t "~&In #'by-word-helper.")
    (if (scan scanner base)
	t
	nil)))

(defun by-word (word source)
  (declare (ignorable word))
  (loop for key being the hash-keys of source
	for val being the hash-values of source
        ;; :do (format t "~&||~A ~%|| ~A" key val)
	:if (or (by-word-helper word val)
                (by-word-helper word key))
	:collect (cons key val)))

(defun country-lookup (key)
  (cond
      ((every #'alpha-char-p key) (by-word key *country-codes*))
      ((every #'alphanumericp key) (by-code key *country-codes*))
      (t "No country for code or term: ~A" key)))

(defun areacode-lookup (key)
  (cond
    ((= (length key) 3) (by-code key *area-codes*))
    ((every #'alpha-char-p key) (by-word key *area-codes*))
    (t (format nil "No landing zone found for your crate-like term: ~A" key))))

(defun airport-lookup (key)
  (cond
    ((= (length key) 3) (by-code key *airport-codes*))
    ((every #'alphanumericp key) (by-word key *airport-codes*))
    (t "No airport found for your puny string: ~A" key)))

(defun currency-lookup (key)
  (cond
    ((every #'alpha-char-p key) (by-word key *currency-codes*))
    (t (format nil "No ISO currency code found for key: ~A" key))))

(defparameter *syllable-counts* nil)

(defun count-syllables ()
  "Read the CMU rhyming dictionary and return a hash mapping words to syllable counts."
  (let ((word->syllables (make-hash-table :test 'equal)))
    (with-open-file (instream (constant-file "cmudict.0.6"))
      (do* ((l (read-line instream nil 'eof) (read-line instream nil 'eof)))
	   ((eq l 'eof) word->syllables)
	(let* ((w (remove #\SPACE (scan-to-strings "^(['A-Z]+)\\s" l)))
	       (count (if w (length (remove-if-not 'digit-char-p l)) 0)))
	  (when w
	    (setf (gethash w word->syllables) count)))))))

(defparameter *8ball-data* nil)

(defun read-8ball ()
  (with-open-file (instream (constant-file "8ball_dump"))
    (do* ((l (read-line instream nil 'eof) (read-line instream nil 'eof))
	  (balls nil))
	 ((eq l 'eof) balls)
      (push l balls))))

(defun consult-8ball ()
  (when (not *8ball-data*)
    (setf *8ball-data* (read-8ball)))
  (random-elt *8ball-data*))

(defparameter *bong-noises* '("HOH" "LGAH" "LAUG" "LUAGH" "UALGHA"
			      "LUAHAGH" "GLUAGH" "HLGHU" "ULGHUG" "MUHLGH"))
