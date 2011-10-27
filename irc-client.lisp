;;;; irc-client.lisp

(in-package #:harlie)

(declaim (optimize (debug 3) (safety 3) (speed 0)))

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
  "if the queue isn't empty, dequeue and send a message, FIFO
   style. Reset the timer for some point 1.5 - 2.5 seconds in the
   future."
  (when (not (queue-empty-p *message-q*))
    (dqmess))
  (sb-ext:schedule-timer *message-timer* (max 1.5 (random 2.5))))

(defparameter *message-timer* (sb-ext:make-timer #'q-runner :name "queue runner." :thread t)
  "this timer empties the message queue as needed.")


(defun print-some-random-dots ()
  "An anti-function function."
  (loop
    for i from 1
    :until (= (random 200) 111)
    :when (= (mod i 3) 0)
    :do (format t ".")
    :finally (return i)))

(defun make-name-detector (name)
  "Return a predicate which detects a token in case-insensitive fashion,
allowing for leading and trailing punctuation characters in the match."
  (lambda (s)
    (scan
     (create-scanner
      (format nil
	      "^[^0-9a-z]*~A[^0-9a-z]*$"
	      name)
      :case-insensitive-mode t) s)))

(defun triggered (token-list sender)
  "Determine whether to trigger an utterance based on something we heard.
If so, return the (possibly rewritten) token list against which to chain
the output.  If not, return nil."
  (let ((recognizer (make-name-detector *my-irc-nick*))
	(trigger-word (find-if (lambda (s) (member s *trigger-list* :test 'string-equal)) token-list)))
    (cond ((remove-if-not recognizer token-list)
	   (mapcar (lambda (s)
		     (if (funcall recognizer s)
			 (regex-replace (string-upcase *my-irc-nick*) (string-upcase s) sender)
			 s))
		   token-list))
	  (trigger-word (progn
			  (setf *trigger-list* (substitute (car (random-words 1)) trigger-word *trigger-list* :test 'string-equal)) 
			  token-list))
	  (t nil))))

(defun start-ignoring (sender)
  "Put sender onto the *ignorelist*.  Returns nil if sender was
on the list already; otherwise returns t."
  (let ((ignoree (string-upcase sender)))
    (if (not (member ignoree *ignorelist* :test 'string=))
	(progn
	  (push ignoree *ignorelist*)
	  t)
	nil)))

(defun stop-ignoring (sender)
  "Take sender off the *ignorelist*.  Returns nil if sender
wasn't on the list; otherwise returns t."
  (let ((ignoree (string-upcase sender)))
    (if (member ignoree *ignorelist* :test 'string=)
	(progn
	  (setf *ignorelist* (remove ignoree *ignorelist* :test 'string=))
	  t)
	nil)))

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
			(command (string-upcase (first token-text-list)))
			(reply-to channel)
			(urls (all-matches-as-strings "((ftp|http|https)://[^\\s]+)|(www[.][^\\s]+)" text))
			(trigger-tokens (triggered token-text-list sender)))

		   (format t "Message: ~A~%" (raw-message-string message))
		   (format t "   connection=~A channel=~A~%" connection channel)

		   (when (equal channel (string-upcase *my-irc-nick*))
		     (setf reply-to sender))

		   (cond ((scan "^NOTIFY:: Help, I'm a bot!" text)
			  (start-ignoring sender)
			  (qmess connection sender "NOTIFY:: Help, I'm a bot!"))

			 ((string= "!IGNOREME" command)
			  (if (or
			       (< (length token-text-list) 2)
			       (not (string= "OFF" (string-upcase (second token-text-list)))))
			      (when (start-ignoring (string-upcase sender))
				(qmess connection reply-to
				       (format nil
					       "Now ignoring ~A.  Use !IGNOREME OFF when you want me to hear you again." sender)))
			      (if (stop-ignoring (string-upcase sender))
				  (qmess connection reply-to
					 (format nil "No longer ignoring ~A." sender))
				  (qmess connection reply-to
					 (format nil "I wasn't ignoring you, ~A." sender)))))
			 
			 ((member (string-upcase sender) *ignorelist* :test 'string=) nil)

			 ((scan "^![^!]" command)
			  (run-plugin (make-instance
				       'plugin-request :botcmd command
						       :connection connection
						       :reply-to reply-to
						       :token-text-list token-text-list)))

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

			 (t (progn
			      (chain-in token-text-list)
			      (when trigger-tokens
				(let ((outgoing (chain (first trigger-tokens) (second trigger-tokens))))
				  (unless (not (mismatch trigger-tokens outgoing :test 'equal))
				    (qmess connection reply-to
					   (format nil "~{~A~^ ~}" outgoing))))))))))))

(defun threaded-byebye-hook (message)
  "Handle a quit or part message."
  (make-thread (lambda ()
		 (setf *last-message* message)
		 (let* ((sender (source message)))
		   (format t "In threaded-byebye-hook, sender = ~A~%" sender)
		   (setf *ignorelist* (remove sender *ignorelist* :test #'equal))))))

(defvar *trigger-list* nil)

(defun start-irc-client-instance ()
  "Start a session with an IRC server."
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
  "Shut down a session with the IRC server."
  (cl-irc:quit *irc-connection*  "I'm tired. I'm going home.")
  (setf *ignorelist* nil)
  (setf *trigger-list* nil)
  (setf *irc-connection* nil))

(defparameter *irc-client-thread* nil)

(defun start-threaded-irc-client-instance ()
  "Spawn a thread to run a session with an IRC server."
  (setf *irc-client-thread* (make-thread #'start-irc-client-instance)))

(defun stop-threaded-irc-client-instance ()
  "Shut down a session with an IRC server, and clean up."
  (stop-irc-client-instance)
  (bt:destroy-thread *irc-client-thread*)
  (setf *irc-client-thread* nil)
  (setf *trigger-list* nil))
