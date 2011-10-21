;;;; harlie.lisp

(in-package #:harlie)

(defvar *connection* nil)

(defvar *last-message* nil)

(defvar *ignorelist* nil)

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
			(sender (source message))
			(connection (connection message))
			(text (second (arguments message)))
			(token-text-list (split "\\s+" text))
			(botcmd (string-upcase (first token-text-list)))
			(reply-to channel))
		   (format t "Message: ~A~%" (raw-message-string message))
		   (format t "   connection=~A channel=~A~%" connection channel)
		   (when (equal channel (string-upcase *my-irc-nick*))
		     (setf reply-to sender))
		   (cond ((member sender *ignorelist* :test #'equal) nil)
			 ((scan "^![^!]" botcmd)
			  (run-plugin botcmd connection reply-to token-text-list))
			 ((scan "^NOTIFY:: Help, I'm a bot!" text)
			  (privmsg connection sender (format nil "NOTIFY:: Help, I'm a bot!"))
			  (push sender *ignorelist*))
			 (t (let ((urls (all-matches-as-strings "((ftp|http|https)://[^\\s]+)|(www[.][^\\s]+)" text)))
			      (when urls
				(dolist (url urls)
				  (unless (scan "127.0.0.1" url)
				    (when (scan "^www" url)
				      (setf url (format nil "http://~A" url)))
				    (destructuring-bind (short title) (lookup-url *the-url-store* url)
				      (if (and short title)
					  (privmsg connection reply-to
						   (format nil "[ ~A~A ] [ ~A ]" *url-prefix* short title))
					  (privmsg connection reply-to
						   (format nil "[ ~A ] Couldn't fetch this page." url))))))))))))))

(defun threaded-byebye-hook (message)
  "Handle a quit or part message."
  (make-thread (lambda ()
		 (setf *last-message* message)
		 (let* ((sender (source message)))
		   (format t "In threaded-byebye-hook, sender = ~A~%" sender)
		   (setf *ignorelist* (remove sender *ignorelist* :test #'equal))))))

(defun run-bot-instance ()
  "Run an instance of the bot."
  (setf *connection* (connect :nickname *my-irc-nick* :server *irc-server-name*))
  (cl-irc:join *connection* "#trinity")
  (privmsg *connection* "#trinity" (format nil "NOTIFY:: Help, I'm a bot!"))
  (add-hook *connection* 'irc::irc-privmsg-message 'threaded-msg-hook)
  (add-hook *connection* 'irc::irc-quit-message 'threaded-byebye-hook)
  (add-hook *connection* 'irc::irc-part-message 'threaded-byebye-hook)
  (read-message-loop *connection*))

(defparameter *bot-thread* nil)

(defun run-bot ()
  "Fork a thread to run an instance of the bot."
  (setf *random-state* (make-random-state t))
  (setf *bot-thread* (make-thread #'run-bot-instance))
  (push (make-instance 'hunchentoot:acceptor :port *web-server-port*) *acceptors*)
  (hunchentoot:start (car *acceptors*))
  (push (create-prefix-dispatcher "/" 'redirect-shortener-dispatch) *dispatch-table*))

(defun kill-bot ()
  "Disconnect from the IRC server, clean up persistent data, shut down the Web servers."
  (cl-irc:quit *connection*  "I'm tired. I'm going home.")
  (dolist (acceptor *acceptors*) (hunchentoot:stop acceptor))
  (setf *acceptors* nil)
  (setf *dispatch-table* (list 'dispatch-easy-handlers 'default-dispatcher))
  (bt:destroy-thread *bot-thread*)
  (setf *bot-thread* nil)
  (setf *ignorelist* nil))
