;;;; harlie.lisp

(in-package #:harlie)

;; With respect to David Gerrold's _When HARLIE Was One_
(defparameter *my-nick* "Harlie")

(defparameter *connection* (connect :nickname *my-nick* :server "irc.srh.org"))

(defparameter *last-message* nil)

; Why do we fork another thread just to run this lambda, you may ask?
; Because the thread that the network event loop runs in keeps getting
; killed every time there's an error in any of this code, and then
; I have to restart the bot, and I get cranky.  That's why.
; This way, the thread that gets killed is an ephemeral thing that no-one
; (well, hardly anyone) will miss.

(defun fetch-title (url)
  (let* ((webtext (http-request url))
	 (document (chtml:parse webtext (cxml-stp:make-builder)))
	 (title "No title found."))
    (stp:do-recursively (a document)
      (when (and (typep a 'stp:element) (equal (stp:local-name a) "title"))
	(setf title (stp:string-value a))))
    title))

(defun threaded-msg-hook (message)
  (make-thread (lambda ()
		 (setf *last-message* message)
		 (let* ((channel (string-upcase (car (arguments message))))
			(connection (connection message))
			(text (second (arguments message)))
			(token-text-list (split "\\s+" text))
			(botcmd (string-upcase (first token-text-list)))
			(reply-to channel))
		   (progn
		     (format t "Message: |~A|~%" (raw-message-string message))
		     (format t "   connection=~A channel=~A~%" connection channel)
		     (if (equal channel (string-upcase *my-nick*))
			 (setf reply-to (user message)))
		     (if (and (scan "^!" botcmd) (not (equal "!" botcmd))) 
			 (cond ((equal botcmd "!SOURCES")
				(privmsg connection reply-to "git@coruscant.deepsky.com:harlie.git"))
			       ((equal botcmd "!STATUS")
				(privmsg connection reply-to "I know no phrases."))
			       (t (privmsg connection reply-to (format nil "~A: unknown command." botcmd))))
			 (let ((urls (all-matches-as-strings "((ftp|http|https)://[^\\s]+)|(www[.][^\\s]+)" text)))
			   (if urls
			       (progn
				 (privmsg connection reply-to "I see URL people:")
				 (format t "~A~%" urls)
				 (dolist (url urls) (privmsg connection reply-to (format nil "   ~A" url))))))))))))

(defun run-bot-instance ()
  (cl-irc:join *connection* "#trinity")
  (add-hook *connection* 'irc::irc-privmsg-message 'threaded-msg-hook)
  (read-message-loop *connection*))

(defun run-bot ()
  (make-thread #'run-bot-instance)
  )
