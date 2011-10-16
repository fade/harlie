;;;; harlie.lisp

(in-package #:harlie)

;; With respect to David Gerrold's _When HARLIE Was One_
(defvar *my-nick* "Harlie")

(defvar *connection* (connect :nickname *my-nick* :server "irc.srh.org"))

(defvar *last-message* nil)

(defvar *urls-by-shortstrings* (make-hash-table :test 'equal :synchronized t))

(defvar *shortstrings-by-urls* (make-hash-table :test 'equal :synchronized t))

(defvar *headlines-by-urls* (make-hash-table :test 'equal :synchronized t))

; There is undoubtedly a better way to extract the text from TITLE tags,
; but this is what we're stuck with for now.

(defun find-title (tree)
  "Search recursively for a :title tag in a nested list, and return the text."
  (if (not (listp tree))
      nil
      ;; If the first element of tree matches :TITLE, then its third element
      ;; ought to be the text we're looking for.
      (if (equal (car tree) :TITLE)
	  (third tree)
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

(defun fetch-title (url)
  "Extract the title from a Web page."
  (multiple-value-bind (webtext status) (http-request url)
    (if (< status 400)
	(let* ((document (chtml:parse webtext (chtml:make-lhtml-builder)))
	       (title "No title found"))
	  (setf title (find-title document))
	  (concatenate 'string (loop for c across title when (not (or (equal c #\Newline) (equal c #\Return)) ) collecting c)))
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
  (apply #'concatenate 'string
	 (loop for i from 1 to *how-short* collecting
				 (string (elt *letterz* (random (length *letterz*)))))))

(defun make-unique-shortstring (url)
  (sb-ext:with-locked-hash-table (*shortstrings-by-urls*)
    (sb-ext:with-locked-hash-table (*urls-by-shortstrings*)
      (do ((short (make-shortstring) (make-shortstring)))
	  ((not (gethash short *urls-by-shortstrings*))
	   (progn
	     (setf (gethash short *urls-by-shortstrings*) url)
	     (setf (gethash url *shortstrings-by-urls*) short)
	     short))))))

(defun lookup-url (url)
  (let ((short (sb-ext:with-locked-hash-table (*shortstrings-by-urls*)
		 (gethash url *shortstrings-by-urls*))))
    (if short
	(sb-ext:with-locked-hash-table (*shortstrings-by-urls*)
	  (list short (gethash url *headlines-by-urls*)))
	(let ((title (fetch-title url)))
	  (if title
	      (progn
		(setf short (make-unique-shortstring url))
		(sb-ext:with-locked-hash-table (*headlines-by-urls*)
		  (setf (gethash url *headlines-by-urls*) title))
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
			       ((equal botcmd "!URL")
				(sb-ext:with-locked-hash-table (*urls-by-shortstrings*)
				  (let ((url (gethash (second token-text-list) *urls-by-shortstrings*)))
				    (if url
					(privmsg connection reply-to (format nil "[ ~A ]" url))
					(privmsg connection reply-to "Sorry, couldn't find a stored URL for that hash.")))))
			       (t (privmsg connection reply-to (format nil "~A: unknown command." botcmd))))
			 (let ((urls (all-matches-as-strings "((ftp|http|https)://[^\\s]+)|(www[.][^\\s]+)" text)))
			   (if urls
			       (progn
				 (format t "~A~%" urls)
				 (dolist (url urls)
				   (destructuring-bind (short title) (lookup-url url)
				     (if (and short title)
					 (privmsg connection reply-to
						  (format nil "[ ~A~A ] [ ~A ]" *url-prefix* short title))
					 (privmsg connection reply-to
						  (format nil "[ ~A ] Couldn't fetch this page." url))))))))))))))

(defun shortener-dispatch ()
  (let ((short (subseq (request-uri*) 1 (1+ *how-short*))))
    (format nil "~A" (gethash short *urls-by-shortstrings* "Nothing found."))))

(defun dump-kruft ()
  (sb-ext:with-locked-hash-table (*shortstrings-by-urls*)
    (sb-ext:with-locked-hash-table (*headlines-by-urls*)
      (let ((foolery
	      (loop for k being the hash-keys in *shortstrings-by-urls* collecting
		     (format nil "<li><a href=\"~A\">~A</A></li>" k (gethash k *headlines-by-urls* "Click here for a random link.")))))
	(concatenate 'string "<html><head><title>Bot Spew</title></head><body><ul>"
		     (apply 'concatenate 'string foolery) "</ul></body></html>")))))

(defun redirect-shortener-dispatch ()
  (let ((uri (request-uri*)))
    (if (> (length uri) *how-short*)
	(let* ((short (subseq (request-uri*) 1 (1+ *how-short*)))
	       (url (gethash short *urls-by-shortstrings*)))
	  (if url
	      (redirect (gethash short *urls-by-shortstrings*))
	      (format nil "<html><head><title>You are in a maze of twisty little redirects, all alike</title></head><body><center><p>With apologies<br>I don't have that URL<br>Perhaps you mistyped?<br></p></center></body></html>")
	      )
	  )
	(dump-kruft))))

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
