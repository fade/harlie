;;;; web-client.lisp

(in-package #:harlie)

; There is undoubtedly a better way to extract the text from TITLE tags,
; but this is what we're stuck with for now.

(defun extract-from-html (tree anchor-p extractor)
  "Recursive traversal of the nested list representation of an HTML tree.
anchor-p is a predicate which will match when we find what we're looking for.
extractor is a function which returns whatever you want from that site.
Only the first match is returned."
  ;; If what we've been handed isn't even a list, then we aren't finding anything here.
  (cond ((not (listp tree)) nil)

	;; Failing that, see if we find what we're looking for right here, and if so,
	;; extract and return a result.

	((apply anchor-p (list tree)) (apply extractor (list tree)))

	;; Otherwise, tree is still a nested list which represents some part
	;; of the document we're looking at.
	;; We consider each sublist of tree beginning with all of tree and
	;; taking the cdr of the current sublist on each iteration.
	;; Also on each iteration, we take the first element in the sublist.
	;; If that element is a list, call ourselves recursively with
	;; that list as the whole tree.
	;; If our sublist has become NIL, or if we find a match, exit.

	(t (let ((found nil))
	     (do* ((sublist tree (cdr sublist))
		   (element (car sublist) (car sublist)))
		  ((or found (not sublist)) found) 
	       (when (listp element)
		 (setf found
		       (extract-from-html element anchor-p extractor))))))))

(defun title-anchor (tree)
  "Predicate which detects a :TITLE tag."
  (and (equal (car tree) :TITLE)
       (> (length tree) 2)))

(defun title-extractor (tree)
  "Extract the text for a :TITLE tag."
  (apply 'concatenate 'string (cddr tree)))

(defun find-title (tree)
  "Search recursively for a :title tag in a nested list, and return the text."
  (extract-from-html tree 'title-anchor 'title-extractor))

(defun forex-anchor (tree)
  "Predicate which detects the meat of an XE.com forex query."
  ;; This the substructure that we're looking for:
  ;;
  ;;        (:TR ((:CLASS "CnvrsnTxt"))
  ;;         (:TD ((:WIDTH "46%") (:ALIGN "right")) "1.00 CAD" "")
  ;;               (:TD ((:WIDTH "8%") (:ALIGN "center") (:CLASS "CnvrsnEq")) "=")
  ;;         (:TD ((:WIDTH "46%") (:ALIGN "left")) "0.980610 USD" ""))
  (and (equal (car tree) :TR)
       (listp (second tree))
       (equal "CnvrsnTxt"
	      (some #'identity
		    (mapcar #'(lambda (proplist)
				(getf proplist :CLASS))
			    (second tree))))))

(defun forex-extractor (tree)
  "Extract the data from an XE.com forex query."
  (list (third (third tree))
	(third (fifth tree))))

(defun find-forex (tree)
  "Plunder a nested list for XE.com's forex information."
  (extract-from-html tree 'forex-anchor 'forex-extractor))

(defparameter *user-agents* '("Mozilla/5.0 (X11; Linux x86_64; rv:7.0.1) Gecko/20100101 Firefox/7.0.1" "Bite Me"))

(defun fetch-title (url)
"Extract the title from a Web page.  Return three values.
If the first value is nil, then no actual title could be found.  If non-nil,
then this value will be the title scraped from the page.
The second value is identical to the first if a title was found.  If not, then
the second value is an informative message to be posted in IRC.
The third value is the redirected URI for the page retrieved, if the page
was retrieved.  If the page couldn't be retrieved, the third value is nil.
This is a very confusing API."
  (let ((page-exists-p nil)
	(store-redirect-uri nil))
    (dolist (user-agent *user-agents*)
      (multiple-value-bind (webtext status nonsense redirect-uri) (webget url :redirect 10 :user-agent user-agent)
	(declare (ignore nonsense))
	(when (and webtext status (< status 400))
	  (setf page-exists-p t
		store-redirect-uri redirect-uri)
	  (if (stringp webtext)
	      (let* ((document (chtml:parse webtext (chtml:make-lhtml-builder)))
		     (title (cleanup-title (find-title document))))
		(when title
		  (return-from fetch-title (values title title redirect-uri))))
	      (return-from fetch-title (values nil "Binary data" redirect-uri))))))
    (values nil (if page-exists-p "No title found" nil) store-redirect-uri)))

;;; alternative scraping system which uses an STP document structure
;;; to do a recursive search on the target document for various
;;; entities and values.

(defun webget (url &rest args)
  "take an URL and get the html returned by the webserver at that
   resource. Return this html to the caller as a string. In the event
   of a network error from drakma, the return of this function is
   NIL. Should not throw to the debugger."
  (unless (member :user-agent args)
    (setf args (append args (list :user-agent (car *user-agents*)))))
  (multiple-value-bind
	(page status headers uri stream winky nod)
      (handler-case
	  (trivial-timeout:with-timeout (15)
	    (ignore-errors
	     (let ((flexi-streams:*substitution-char* #\?))
	       (apply #'drakma:http-request url
		      args))))
	(trivial-timeout:timeout-error (c) (declare (ignore c)) nil))
    (declare (ignorable page status headers uri stream winky nod))
    (if (every #'identity (list page status headers uri winky nod))
	(values page status headers uri winky nod)
	(values nil nil))))

(defun clean-html (string)
  "take a shot at repairing broken html. This won't work in the
   presence of semantically undecidable breakage, but it does work for
   the most common types of human fuck-up."
  (chtml:parse string (chtml:make-string-sink)))

(defun parse-page (page)
  "assumes page refers to a string of raw html. Returns a cxml-stp
   document structure."
  (chtml:parse (clean-html page) (cxml-stp:make-builder)))

(defun stp->string (stpdoc)
  (stp:serialize stpdoc (cxml:make-string-sink)))

(defun dump-page-string (page file)
  (with-open-file (st file :direction :output :if-exists :supersede)
    (write-string page st)))

;;; /webtils

(defun calc-anchor (tree)
  "Predicate which detects the result in a Google Calc page."
  (and (equal (car tree) :H2)
       (listp (second tree))
       (equal "r"
	      (some #'identity
		    (mapcar #'(lambda (proplist)
				(getf proplist :CLASS))
			    (second tree))))))

(defun cleanup-calc-string (s)
  (string-strip-surrounding-whitespace
   (substitute #\Space #\NO-BREAK_SPACE
	       (substitute #\x #\MULTIPLICATION_SIGN s))))

(defun calc-extractor (tree)
  "Extract the result from a Google Calc query."
  (let* ((payload (subseq tree 2))
	 (pivot (position #\= (car payload)))
	 (lhs (cleanup-calc-string (subseq (car payload) 0 pivot)))
	 (mantissa (cleanup-calc-string (subseq (car payload) (1+ pivot))))
	 (trailer (if (> (length payload) 1)
		      (concatenate 'string " " (cleanup-calc-string (third payload))) 
		      nil)))
    (cond ((= 1 (length payload))
	   (format nil "~A = ~A" lhs mantissa))
	  ((= 3 (length payload))
	   (let ((exponent (third (second payload))))
	     (list (format nil "~V@A~A" (+ (length lhs) (length " = ") (length mantissa)) "" exponent)
		   (format nil "~A = ~A~V@A~A" lhs mantissa (length exponent) "" trailer))))
	  ((= 5 (length payload))
	   (let* ((numerator (third (second payload)))
		  (denominator (third (fourth payload)))
		  (trailing-unit (fifth payload))
		  (thruline (format nil "~A = ~A ~A/~A~A" lhs mantissa numerator denominator trailing-unit)))
	     thruline))
	  (t "I dunno, man, I just work here."))))

(defun find-calc (tree)
  "Take the result in a Google Calc page."
  (let ((result (extract-from-html tree 'calc-anchor 'calc-extractor)))
    (if result result
	"Your query did not return any meaningful data.")))

(defun doomsday-anchor (tree)
  "Predicate which detects the result in a Doomsday lookup."
  (and (eq (car tree) :DIV)
       (>= (length tree) 3)
       (listp (second tree))
       (string= (getf (caadr tree) :CLASS) "module-content")
       (listp (third tree))
       (eq (car (third tree)) :H3)))

(defun doomsday-extractor (tree)
  "Extract the result from a Doomsday lookup."
  (third (third tree)))

(defun find-doomsday (tree)
  "Find how many minutes to midnight according to the Bulletin of the Atomic Scientists."
  (extract-from-html tree 'doomsday-anchor 'doomsday-extractor))

(defun twitter-twit (url)
  "Convenience function to extract the Twitter user name from an URL."
  (multiple-value-bind (whole parts) (scan-to-strings "twitter.com/#!/([^/]+)/status/([0-9]+)" url)
    (if whole
	(elt parts 0)
	nil)))

(defun twitter-payload (url)
  "Fetch the text from a Twitter posting."
  (let ((parts (scan-to-substrings "twitter.com/(#!/)?([^/]+)/status/([0-9]+)" url)))
    (if parts
	(let* ((twit (elt parts 1))
	       (twit-id-string (elt parts 2))
	       (twitter-spooge (with-open-stream
				   (s (drakma:http-request
				       (format nil "http://search.twitter.com/search.json?q=from:~A&max_id=~A" twit twit-id-string)
				       :want-stream t))
				 (json:decode-json-from-source s))))
	  (cdr (assoc :text (car (remove-if-not #'(lambda (x) (string= (cdr (assoc :id--str x)) twit-id-string))
						(cdr (assoc :results twitter-spooge)))))))
	nil)))

;; drakma is very thorough in checking the correctness of the HTML
;; it fetches.  Unfortunately, it wants to see a newline character
;; at the end-of-file.  The Hacker News website doesn't provide one.
;; *old-bogus-eols* stores the original value of
;; chunga:*accept-bogus-eols* so we can set it to t to suppress
;; this stringent checking.

(defvar *old-bogus-eols* nil)
(defvar *old-external-format* nil)

(defun start-web-client ()
  "Initialize the system to request Web pages."
  (setf *old-bogus-eols* chunga:*accept-bogus-eols*)
  (setf chunga:*accept-bogus-eols* t)
  (setf *old-external-format* drakma:*drakma-default-external-format*)
  (setf drakma:*drakma-default-external-format* :utf-8))

(defun stop-web-client ()
  "Clean up after requesting Web pages."
  (setf chunga:*accept-bogus-eols* *old-bogus-eols*)
  (setf *old-bogus-eols* nil)
  (setf drakma:*drakma-default-external-format* *old-external-format*))
