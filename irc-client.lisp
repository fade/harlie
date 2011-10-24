;;;; irc-client.lisp

(in-package #:harlie)

(defvar *irc-connection* nil)

(defvar *last-message* nil)

(defvar *ignorelist* nil)

(defparameter *message-q* (make-queue :name "message queue")
  "a global queue to hold all bot messaging output so that we can
  rate-limit it.")

(defun qmess (connection reply-to message)
  "queue a message for rate-limited sending."
  (enqueue (list connection reply-to message) *message-q*))

(defun dqmess ()
  "dequeue a message to send."
  (let* ((mobj (dequeue *message-q*))
	 (connection (first mobj))
	 (reply-to (second mobj))
	 (message (third mobj)))
    (privmsg connection reply-to message)))

(defun q-runner ()
  "if the queue isn't empty, dequeue and send a message, FIFO style."
  (when (not (queue-empty-p *message-q*))
    (dqmess))
  (sb-ext:schedule-timer *message-timer* (max 1.5 (random 2.5))))

(defparameter *message-timer* (sb-ext:make-timer #'q-runner :name "queue runner."))

(defun print-some-random-dots ()
  "An anti-function function."
  (loop
    for i from 1
    :until (= (random 200) 111)
    :when (= (mod i 3) 0)
    :do (format t ".")
    :finally (return i)))

(defun make-name-detector (name)
  (lambda (s)
    (scan
     (create-scanner
      (format nil
	      "^[^0-9a-z]*~A[^0-9a-z]*$"
	      name)
      :case-insensitive-mode t) s)))

(defun triggered (token-list sender)
  (let ((recognizer (make-name-detector *my-irc-nick*))
	(trigger-word (find-if (lambda (s) (member s *trigger-list* :test 'equal)) token-list)))
    (cond ((remove-if-not recognizer token-list)
	   (mapcar (lambda (s)
		     (if (funcall recognizer s)
			 (regex-replace (string-upcase *my-irc-nick*) (string-upcase s) sender)
			 s))
		   token-list))
	  (trigger-word (progn
			  (setf *trigger-list* (substitute (car (random-words 1)) trigger-word *trigger-list* :test 'equal)) 
			  token-list))
	  (t nil))))

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
			(reply-to channel)
			(urls (all-matches-as-strings "((ftp|http|https)://[^\\s]+)|(www[.][^\\s]+)" text))
			(trigger-tokens (triggered token-text-list sender)))
		   (format t "Message: ~A~%" (raw-message-string message))
		   (format t "   connection=~A channel=~A~%" connection channel)
		   (when (equal channel (string-upcase *my-irc-nick*))
		     (setf reply-to sender))
		   (cond ((member sender *ignorelist* :test #'equal) nil)
			 ((scan "^![^!]" botcmd)
			  (run-plugin botcmd connection reply-to token-text-list))
			 ((scan "^NOTIFY:: Help, I'm a bot!" text)
			  (qmess connection sender (format nil "NOTIFY:: Help, I'm a bot!"))
			  (push sender *ignorelist*))
			 (urls
			  (dolist (url urls)
			    (unless (or (scan (format nil "http://~A:~A" *web-server-name* *web-server-port*) url)
					(scan "127.0.0.1" url))
			      (when (scan "^www" url)
				(setf url (format nil "http://~A" url)))
			      (destructuring-bind (short title) (lookup-url *the-url-store* url sender)
				(if (and short title)
				    (qmess connection reply-to
					   (format nil "[ ~A~A ] [ ~A ]" *url-prefix* short title))
				    (qmess connection reply-to
					   (format nil "[ ~A ] Couldn't fetch this page." url)))))))
			 (trigger-tokens
			  (qmess connection reply-to (format nil "~{~A~^ ~}" (chain (first trigger-tokens) (second trigger-tokens)))))
			 (t nil))))))

(defun threaded-byebye-hook (message)
  "Handle a quit or part message."
  (make-thread (lambda ()
		 (setf *last-message* message)
		 (let* ((sender (source message)))
		   (format t "In threaded-byebye-hook, sender = ~A~%" sender)
		   (setf *ignorelist* (remove sender *ignorelist* :test #'equal))))))

(defvar *trigger-list* nil)

(defun start-irc-client-instance ()
  "Run an instance of the bot's IRC client."
  (setf *irc-connection* (connect :nickname *my-irc-nick* :server *irc-server-name*))
  (setf *trigger-list* (random-words 10))
  (dolist (channel *irc-channel-names*)
    (cl-irc:join *irc-connection* channel)
    (privmsg *irc-connection* channel (format nil "NOTIFY:: Help, I'm a bot!")))
  (sb-ext:schedule-timer *message-timer* 5)  
  (add-hook *irc-connection* 'irc::irc-privmsg-message 'threaded-msg-hook)
  (add-hook *irc-connection* 'irc::irc-quit-message 'threaded-byebye-hook)
  (add-hook *irc-connection* 'irc::irc-part-message 'threaded-byebye-hook)
  (read-message-loop *irc-connection*))

(defun stop-irc-client-instance ()
  (cl-irc:quit *irc-connection*  "I'm tired. I'm going home.")
  (setf *ignorelist* nil)
  (setf *trigger-list* nil)
  (setf *irc-connection* nil))

(defparameter *irc-client-thread* nil)

(defun start-threaded-irc-client-instance ()
  (setf *irc-client-thread* (make-thread #'start-irc-client-instance)))

(defun stop-threaded-irc-client-instance ()
  (stop-irc-client-instance)
  (bt:destroy-thread *irc-client-thread*)
  (setf *irc-client-thread* nil)
  (setf *trigger-list* nil))
