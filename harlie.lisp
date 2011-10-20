;;;; harlie.lisp

(in-package #:harlie)

(defvar *connection* nil)

(defvar *last-message* nil)

(defvar *the-url-store* (make-instance 'hash-url-store))

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
	       (if (listp element)
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
		    (mapcar (lambda (proplist)
			      (getf proplist :CLASS))
			    (second tree))))))

(defun forex-extractor (tree)
  "Extract the data from an XE.com forex query."
  (list (third (third tree))
	(third (fifth tree))))

(defun find-forex (tree)
  "Plunder a nested list for XE.com's forex information."
  (extract-from-html tree 'forex-anchor 'forex-extractor))

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

(defun fetch-formatted-url (url-string &rest args)
  "Retrieve the lhtml contents of the page at a specified URL.
Does format-style string interpolation on the url string."
  (chtml:parse
   (http-request
    (apply 'format nil url-string args))
   (chtml:make-lhtml-builder)))

(defvar *plugins* nil)

(defmacro defplugin (funame args &rest body)
  `(push (cons (symbol-name (quote ,funame)) (lambda (,@args) ,@body)) *plugins*))

(defplugin sources (connection reply-to token-list)
  (declare (ignore token-list))
  (privmsg connection reply-to "git@coruscant.deepsky.com:harlie.git"))

(defplugin status (connection reply-to token-list)
  (declare (ignore token-list))
  (privmsg connection reply-to "I know no phrases."))

(defplugin conv (connection reply-to token-list)
  (let* ((amount (second token-list))
	 (from (third token-list))
	 (to (fourth token-list))
	 (forex (find-forex (fetch-formatted-url
			     "http://www.xe.com/ucc/convert/?Amount=~A&From=~A&To=~A"
			     amount from to))))
    (privmsg connection reply-to (format nil "~A = ~A" (first forex) (second forex)))))

(defplugin jcw (connection reply-to token-list)
  (declare (ignore token-list))
  (privmsg connection reply-to (format nil "FUCK YOU, JACKHOLE!")))

(defplugin rally (connection reply-to token-list)
  (declare (ignore token-list))
  (privmsg connection reply-to (format nil "FUCK YOU, HANS!")))

(defplugin f1 (connection reply-to token-list)
  (declare (ignore token-list))
  (privmsg connection reply-to (format nil "FUCK YOU, SCHUMACHER!")))

(defun run-plugin (botcmd connection reply-to token-list)
  (let* ((plugname (string-upcase (subseq botcmd 1)))
	 (plugf (assoc plugname *plugins* :test #'string=)))
    (format t "~A~%" plugname)
    (format t "~A~%" plugf)
    (format t "~A~%" *plugins*)
    (if plugf
	(funcall (cdr plugf) connection reply-to token-list)
	(privmsg connection reply-to (format nil "~A: unknown command." plugname)))))

; Why do we fork another thread just to run this lambda, you may ask?
; Because the thread that the network event loop runs in keeps getting
; killed every time there's an error in any of this code, and then
; I have to restart the bot, and I get cranky.  That's why.
; This way, the thread that gets killed is an ephemeral thing that no-one
; (well, hardly anyone) will miss.

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
		   (format t "Message: ~A~%" (raw-message-string message))
		   (format t "   connection=~A channel=~A~%" connection channel)
		   (if (equal channel (string-upcase *my-irc-nick*))
		       (setf reply-to (user message)))
		   (if (scan "^![^!]" botcmd)
		       (run-plugin botcmd connection reply-to token-text-list)
		       (let ((urls (all-matches-as-strings "((ftp|http|https)://[^\\s]+)|(www[.][^\\s]+)" text)))
			 (if urls
			     (dolist (url urls)
			       (destructuring-bind (short title) (lookup-url *the-url-store* url)
				 (if (and short title)
				     (privmsg connection reply-to
					      (format nil "[ ~A~A ] [ ~A ]" *url-prefix* short title))
				     (privmsg connection reply-to
					      (format nil "[ ~A ] Couldn't fetch this page." url))))))))))))

(defgeneric make-webpage-listing-urls (store)
  (:documentation "Generate and return the HTML for a page listing all the URLS in the store."))

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
	(let* ((short (subseq (request-uri*) 1))
	       (url (get-url-from-shortstring *the-url-store* short)))
	  (if url
	      (redirect url)
	      (html-apology)))
	(make-webpage-listing-urls *the-url-store*))))

(defun run-bot-instance ()
  "Run an instance of the bot."
  (setf *connection* (connect :nickname *my-irc-nick* :server *irc-server-name*))
  (cl-irc:join *connection* "#trinity")
  (add-hook *connection* 'irc::irc-privmsg-message 'threaded-msg-hook)
  (read-message-loop *connection*))

(defparameter *bot-thread* nil)

(defun run-bot ()
  "Fork a thread to run an instance of the bot."
  (setf *random-state* (make-random-state t))
  (setf *bot-thread* (make-thread #'run-bot-instance))
  (hunchentoot:start (make-instance 'hunchentoot:acceptor :port *web-server-port*))
  (push (create-prefix-dispatcher "/" 'redirect-shortener-dispatch) *dispatch-table*))

(defun kill-bot ()
  (cl-irc:quit *connection*  "I'm tired. I'm going home.")
  (bt:destroy-thread *bot-thread*)
  (setf *bot-thread* nil))
