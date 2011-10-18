;;;; harlie.lisp

(in-package #:harlie)

;; With respect to David Gerrold's _When HARLIE Was One_
(defvar *my-nick* "Harlie")

(defvar *connection* (connect :nickname *my-nick* :server "irc.srh.org"))

(defvar *last-message* nil)

(defclass url-store () ())

(defclass hash-url-store (url-store)
  ((url->short :initform (make-hash-table :test 'equal :synchronized t) :accessor url->short)
   (short->url :initform (make-hash-table :test 'equal :synchronized t) :accessor short->url)
   (url->headline :initform (make-hash-table :test 'equal :synchronized t) :accessor url->headline)))

(defvar *the-url-store* (make-instance 'hash-url-store))

(defgeneric make-unique-shortstring (store url)
  (:documentation "Assign a new short URL string to URL."))

(defgeneric lookup-url (store url)
  (:documentation "Return present or new short URL and title for specified URL."))

(defgeneric make-webpage-listing-urls (store)
  (:documentation "Generate and return the HTML for a page listing all the URLS in the store."))

(defgeneric get-url-from-shortstring (store short)
  (:documentation "Return the full URL associated with a given short string."))

(defmethod get-url-from-shortstring ((store hash-url-store) short)
  (sb-ext:with-locked-hash-table ((short->url store))
    (gethash short (short->url store))))

; There is undoubtedly a better way to extract the text from TITLE tags,
; but this is what we're stuck with for now.

(defun string-strip-surrounding-whitespace (str)
  "Strip whitespace characters from beginning and end of a string."
  (string-trim '(#\Space #\Newline #\Return #\Tab) str))

(defun string-remove-embedded-newlines (str)
  "Remove newline or carriage return characters from a string."
  (concatenate 'string (loop for c across str when (not (or (eq c #\Newline) (eq c #\Return))) collecting c)))

(defun cleanup-title (title)
  "Remove extraneous whitespace characters from within and around a string."
  (string-strip-surrounding-whitespace (string-remove-embedded-newlines title)))

(defun find-title (tree)
  "Search recursively for a :title tag in a nested list, and return the text."
  (if (not (listp tree))
      nil
      ;; If the first element of tree matches :TITLE, then its third and
      ;; subsequent elements ought to be the text we're looking for.
      (if (and  (equal (car tree) :TITLE) (> (length tree) 2))
	  (apply 'concatenate 'string (cddr tree))
	  ;; Otherwise, tree is still a nested list which represents some part
	  ;; of the document we're looking at.
	  ;; We consider each sublist of tree beginning with all of tree and
	  ;; taking the cdr of the current sublist on each iteration.
	  ;; Also on each iteration, we take the first element in the sublist.
	  ;; If that element is a list, call ourselves recursively with
	  ;; that list as the whole tree.
	  ;; If our sublist has become NIL, or if we find a match, exit.
	  ;; So if some bastard has put two <title> tags in, we'll only find
	  ;; the first one.  Oh well.
	  ;; There's undoubtedly some pathological case where somebody creates
	  ;; an attribute called "title" that'll fuck us up, but we'll fix that
	  ;; when it comes up.
	  (let ((found nil))
	    (do* ((sublist tree (cdr sublist))
		  (element (car sublist) (car sublist)))
		 ((or found (not sublist)) found) 
	      (if (listp element) (setf found (find-title element))))))))

(defun find-forex (tree)
  "Plunder a nested list for XE.com's forex information."
  (if (not (listp tree))
      nil
      ;; If the first element of tree matches :TITLE, then its third and
      ;; subsequent elements ought to be the text we're looking for.
      (if (and (equal (car tree) :TR)
	       (listp (second tree))
	       (listp (car (second tree)))
	       (eq (car (car (second tree))) :CLASS)
	       (equal (cadar (second tree)) "CnvrsnTxt"))
	  (list (third (third tree)) (third (fifth tree)))
	  ;; Otherwise, tree is still a nested list which represents some part
	  ;; of the document we're looking at.
	  ;; We consider each sublist of tree beginning with all of tree and
	  ;; taking the cdr of the current sublist on each iteration.
	  ;; Also on each iteration, we take the first element in the sublist.
	  ;; If that element is a list, call ourselves recursively with
	  ;; that list as the whole tree.
	  ;; If our sublist has become NIL, or if we find a match, exit.
	  ;; So if some bastard has put two <title> tags in, we'll only find
	  ;; the first one.  Oh well.
	  ;; There's undoubtedly some pathological case where somebody creates
	  ;; an attribute called "title" that'll fuck us up, but we'll fix that
	  ;; when it comes up.
	  (let ((found nil))
	    (do* ((sublist tree (cdr sublist))
		  (element (car sublist) (car sublist)))
		 ((or found (not sublist)) found) 
	      (if (listp element) (setf found (find-forex element))))))))

(defun fetch-title (url)
  "Extract the title from a Web page."
  (multiple-value-bind (webtext status) (http-request url)
    (if (< status 400)
	(let* ((document (chtml:parse webtext (chtml:make-lhtml-builder)))
	       (title (find-title document)))
	  (if title
	      (cleanup-title title)
	      "No title found."))
	nil)))

; Why do we fork another thread just to run this lambda, you may ask?
; Because the thread that the network event loop runs in keeps getting
; killed every time there's an error in any of this code, and then
; I have to restart the bot, and I get cranky.  That's why.
; This way, the thread that gets killed is an ephemeral thing that no-one
; (well, hardly anyone) will miss.

(defparameter *letterz* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defparameter *how-short* 5)

(defun make-shortstring ()
  "Generate a single random short-URL string."
  (apply #'concatenate 'string
	 (loop for i from 1 to *how-short* collecting
					   (string (elt *letterz* (random (length *letterz*)))))))

(defmethod make-unique-shortstring ((store hash-url-store) url)
  (sb-ext:with-locked-hash-table ((short->url store))
    (sb-ext:with-locked-hash-table ((url->short store))
      (do ((short (make-shortstring) (make-shortstring)))
	  ((not (gethash short (short->url store)))
	   (progn
	     (setf (gethash short (short->url store)) url)
	     (setf (gethash url (url->short store)) short)
	     short))))))

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

(defparameter *url-server-port* 5791)
(defparameter *url-prefix* (format nil "http://127.0.0.1:~A/" *url-server-port*) )

(defun threaded-msg-hook (message)
  "Handle an incoming message."
  (make-thread (lambda ()
		 (setf *last-message* message)
		 (let* ((channel (string-upcase (car (arguments message))))
			(connection (connection message))
			(text (second (arguments message)))
			(token-text-list (split "\\s+" text))
			(botcmd (string-upcase (first token-text-list)))
			(reply-to channel))
		   (progn
		     (format t "Message: ~A~%" (raw-message-string message))
		     (format t "   connection=~A channel=~A~%" connection channel)
		     (if (equal channel (string-upcase *my-nick*))
			 (setf reply-to (user message)))
		     (if (and (scan "^!" botcmd) (not (equal "!" botcmd))) 
			 (cond ((equal botcmd "!SOURCES")
				(privmsg connection reply-to "git@coruscant.deepsky.com:harlie.git"))
			       ((equal botcmd "!STATUS")
				(privmsg connection reply-to "I know no phrases."))
			       ((equal botcmd "!CONV")
				(let* ((amount (second token-text-list))
				       (from (third token-text-list))
				       (to (fourth token-text-list))
				       ;; the (chtml:parse ...) rigmarole should be split into a utility function. we'll be using it quite a lot.
				       (forex (find-forex (chtml:parse
							   (http-request
							    (format nil "http://www.xe.com/ucc/convert/?Amount=~A&From=~A&To=~A" amount from to))
							   (chtml:make-lhtml-builder)))))
				  (privmsg connection reply-to (format nil "~A = ~A" (first forex) (second forex)))))
			       
			       (t (privmsg connection reply-to (format nil "~A: unknown command." botcmd))))
			 (let ((urls (all-matches-as-strings "((ftp|http|https)://[^\\s]+)|(www[.][^\\s]+)" text)))
			   (if urls
			       (progn
				 (format t "~A~%" urls)
				 (dolist (url urls)
				   (destructuring-bind (short title) (lookup-url *the-url-store* url)
				     (if (and short title)
					 (privmsg connection reply-to
						  (format nil "[ ~A~A ] [ ~A ]" *url-prefix* short title))
					 (privmsg connection reply-to
						  (format nil "[ ~A ] Couldn't fetch this page." url))))))))))))))

(defmethod make-webpage-listing-urls ((store hash-url-store))
  (sb-ext:with-locked-hash-table ((url->short store))
    (sb-ext:with-locked-hash-table ((url->headline store))
      (let ((foolery
	      (loop for k being the hash-keys in (url->short store) collecting
									(format nil "<li><a href=\"~A\">~A</A></li>" k (gethash k (url->headline store) "Click here for a random link.")))))
	(concatenate 'string "<html><head><title>Bot Spew</title></head><body><ul>"
		     (apply 'concatenate 'string foolery) "</ul></body></html>")))))

(defun html-apology ()
  (format nil "
<html>
<head>
<title>You are in a maze of twisty little redirects, all alike</title>
</head>
<body>
<center>
<p>With apologies<br>
I don't have that URL<br>
Perhaps you mistyped?<br>
</p>
</center>
</body>
</html>"))

(defun redirect-shortener-dispatch ()
  "Dispatcher for the Web pages served by the bot.
Serve up a redirect page, a list of shortened URL links,
or an error message, as appropriate."
  (let ((uri (request-uri*)))
    (if (> (length uri) *how-short*)
	(let* ((short (subseq (request-uri*) 1 (1+ *how-short*)))
	       (url (get-url-from-shortstring *the-url-store* short)))
	  (if url
	      (redirect url)
	      (html-apology)))
	(make-webpage-listing-urls *the-url-store*))))

(defun run-bot-instance ()
  "Run an instance of the bot."
  (cl-irc:join *connection* "#trinity")
  (add-hook *connection* 'irc::irc-privmsg-message 'threaded-msg-hook)
  (read-message-loop *connection*))

(defun run-bot ()
  "Fork a thread to run an instance of the bot."
  (setf *random-state* (make-random-state t))
  (make-thread #'run-bot-instance)
  (hunchentoot:start (make-instance 'hunchentoot:acceptor :port *url-server-port*))
  (push (create-prefix-dispatcher "/" 'redirect-shortener-dispatch) *dispatch-table*)
  )
