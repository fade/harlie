;;;; irc-client.lisp

(in-package #:harlie)

(declaim (optimize (debug 3) (safety 3) (speed 0)))

(defvar *irc-connections* (make-synchronized-hash-table :test 'equal))

(defun some-connection ()
  "Sometimes you just want to grab a connection object from the REPL."
  (car (hash-table-values *irc-connections*)))

(defun make-message-queue ()
  (make-instance 'jpl-queues:synchronized-queue
		 :queue (make-instance 'jpl-queues:unbounded-fifo-queue)))

;; We subclass cl-irc:connection and cl-irc:channel so we can store per-connection
;; and per-channel data here.

(defclass bot-irc-connection (cl-irc:connection)
  ((last-message :initform nil :accessor last-message)
   (bot-irc-client-thread :initform nil :accessor bot-irc-client-thread)
   (ignore-list :initform nil :accessor ignore-list)
   (message-q :initform (make-message-queue) :accessor message-q)
   ;;   (message-timer :initform nil :accessor message-timer)
   (mq-task :initform nil :accessor mq-task)))

(defclass bot-irc-channel (cl-irc:channel)
  ((trigger-list :initarg :trigger-list :initform nil :accessor trigger-list)))

;; Here, we insinuate ourselves into the cl-irc plumbing by subclassing the irc channel
;; on the fly and setting up the trigger list.

(defmethod cl-irc::default-hook :after ((message cl-irc:irc-join-message))
  (let* ((connection (cl-irc:connection message))
	 (channel-name (car (arguments message)))
	 (channel (find-channel connection channel-name)))
    (change-class channel 'bot-irc-channel
		  :trigger-list (random-words
				 (make-instance 'bot-context
						:bot-nick (nickname (user connection))
						:bot-irc-server (server-name connection)
						:bot-irc-channel channel-name)
				 10 #'acceptable-word-p))))

;; The rate-limiting queue structure for the connection to the IRC server.

(defparameter *mess-count* 0)

(defun make-mq-thunk (connection)
  "A closure to generate the function which gets called periodically to flush the queue."
  #'(lambda ()
      (format t "Entering the inner thunk.~%")
      (if (not (jpl-queues:empty? (message-q connection)))
	  (dqmess connection)
	  (setf (task-interval (mq-task connection)) nil))))

(defun start-irc-message-queue (connection)
  "A convenience function to regenerate the queue flusher and restart it."
  (setf (mq-task connection) (make-task (make-mq-thunk connection) nil "queue runner.")))

(defgeneric qmess (connection reply-to message)
  (:documentation "queue a message for rate-limited sending."))

(defgeneric dqmess (connection)
  (:documentation "dequeue a message to send."))

(defmethod qmess ((connection bot-irc-connection) reply-to message)
  (incf *mess-count*)
  (let ((task (mq-task connection)))
    (when (null (task-interval task))
      (start-task task (max 1.5 (random 2.5))))
    (jpl-queues:enqueue (list reply-to message) (message-q connection))))

(defmethod dqmess ((connection bot-irc-connection))
  (let* ((mobj (jpl-queues:dequeue (message-q connection)))
	 (reply-to (first mobj))
	 (message (second mobj)))
    (format t "~&replying to: ~A~% with [~:D]: ~A~%" reply-to *mess-count* message)
    (privmsg connection reply-to message)))

(defmethod initialize-instance :after ((connection bot-irc-connection) &key)
  (start-irc-message-queue connection))

;; The !IGNOREME plumbing.

(defgeneric start-ignoring (connection sender)
  (:documentation "Begin ignoring sender on connection.  Returns nil if
sender was already being ignored, and a true value otherwise."))

(defgeneric stop-ignoring (connection sender)
  (:documentation "Stop ignoring sender on connection.  Returns nil if
sender wasn't being ignored; true otherwise."))

(defmethod start-ignoring ((connection bot-irc-connection) sender)
  (let ((ignoree (string-upcase sender)))
    (if (not (member ignoree (ignore-list connection) :test #'string-equal))
        (push ignoree (ignore-list connection))
        nil)))

(defmethod stop-ignoring ((connection bot-irc-connection) sender)
  (let ((ignoree (string-upcase sender)))
    (if (member ignoree (ignore-list connection) :test #'string-equal)
	(progn
	  (setf (ignore-list connection) (remove ignoree (ignore-list connection) :test #'string-equal))
	  t)
	nil)))

(defun ignoring-whom ()
  "Convenience function to print out some semblance of who's on the global ignore list."
  (maphash #'(lambda (k v) (format t "~A ~A~%" k (ignore-list v))) *irc-connections*))

(defun print-some-random-dots ()
  "An anti-function function."
  (loop
    for i from 1
    :until (= (random 200) 111)
    :when (= (mod i 3) 0)
      :do (format t ".")
    :finally (return i)))

;; Two functions used for the triggering mechanism about the chainer.

(defun make-name-detector (name)
  "Return a predicate which detects a token in case-insensitive fashion,
allowing for leading and trailing punctuation characters in the match."
  #'(lambda (s)
      (scan
       (create-scanner
	(format nil
		"^[^0-9a-z]*~A[^0-9a-z]*$"
		name)
	:case-insensitive-mode t) s)))

(defun triggered (context token-list sender channel)
  "Determine whether to trigger an utterance based on something we heard.
   If so, return the (possibly rewritten) token list against which to chain
   the output.  If not, return nil."
  (let ((recognizer (make-name-detector (bot-nick context)))
	(trigger-word (find-if #'(lambda (s) (member s token-list :test #'string-equal)) (trigger-list channel))))
    (cond ((> 10 (length (trigger-list channel)))
	   (progn
	     (format t "[~&in triggered] ~A~%" (trigger-list channel))
	     (setf (trigger-list channel) (append (trigger-list channel) token-list))
	     (format t "[~&after append token list in triggered] ~A~%" (trigger-list channel))
	     (when (< 10 (length (trigger-list channel)))
	       (setf (trigger-list channel) (subseq (trigger-list channel) 0 10)))
	     nil)
	   (remove-if-not recognizer token-list)
	   (mapcar #'(lambda (s)
		       (if (funcall recognizer s)
			   (regex-replace (string-upcase (bot-nick context)) (string-upcase s) sender)
			   s))
		   token-list))
	  (trigger-word
	   (progn
	     (setf (trigger-list channel)
		   (remove trigger-word (trigger-list channel) :test #'string=))
	     (setf (trigger-list channel)
		   (append (trigger-list channel)
			   (random-words context
					 (- 10 (length (trigger-list channel)))
					 #'acceptable-word-p)))
	     token-list))
	  (t nil))))

(defun extract-urls (text)
  (all-matches-as-strings "((http|https)://[^\\s]+)|(www[.][^\\s.][^\\s]*[.][^\\s.][^\\s]*)" text))

(defun ignoring (connection sender text reply)
  "Apply commands related to ignoring/not ignoring the speaker.  Return t if
   the rest of the bot should ignore this utterance; return nil otherwise."
  (let ((ignore-phrase "NOTIFY:: Help, I'm a bot!"))
    (cond ((string= ignore-phrase text)
	   (when (start-ignoring connection sender)
	     (privmsg connection sender ignore-phrase)))
	  ((string-equal text "!ignoreme off")
	   (if (stop-ignoring connection sender)
	       (funcall reply (format nil "No longer ignoring ~A." sender))
	       (funcall reply (format nil "I wasn't ignoring you, ~A." sender))))
	  ((scan (create-caseless-scanner "^!ignoreme") text)
	   (when (start-ignoring connection sender)
	     (funcall reply (format nil "Now ignoring ~A.  Use !IGNOREME OFF when you want me to hear you again." sender))))
	  ;; If there wasn't an ignore toggle command, look up the speaker's ignore status and return it.
	  (t (return-from ignoring (member sender (ignore-list connection) :test #'string-equal))))
    ;; As there was an ignore toggle command, it's been handled and so should be ignored.
    t))

(defun msg-hook (message action)
  "Handle an incoming message."
  (let* ((connection (connection message))
	 (channel-name (car (arguments message)))
	 (channel (gethash channel-name (channels connection) nil))
	 (sender (source message))
	 (text (regex-replace-all "\\ca" (second (arguments message)) ""))
	 (token-text-list (split "\\s+" text))
	 (command (string-upcase (first token-text-list)))
	 (context (make-instance
		   'bot-context
		   :bot-nick (nickname (user connection))
		   :bot-irc-server (server-name connection)
		   :bot-irc-channel channel))
	 (reply-to (if (string-equal channel-name (bot-nick context))
		       sender
		       channel-name))
	 (urls (mapcar #'de-utm-url (extract-urls text))))
    (flet ((reply (s) (qmess connection reply-to s)))
      (setf (last-message connection) message)
      (format t "Message: ~A~%" (raw-message-string message))
      (format t "   connection=~A channel=~A~%" connection channel-name)

      (unless (ignoring connection sender text #'reply)
	(cond ((scan "^![^!]" command)
	       (run-plugin (make-instance
			    'plugin-request :botcmd command
					    :plugin-context context
					    :connection connection
					    :channel-name channel-name
					    :reply-to reply-to
					    :token-text-list token-text-list)))
	      (urls
	       (dolist (url urls)
		 (unless (or (scan (make-short-url-string context "") url)
			     (scan "127.0.0.1" url))
		   (when (scan "^www" url)
		     (setf url (format nil "http://~A" url)))
		   (multiple-value-bind (short title) (lookup-url *the-url-store* context url sender)
		     (if (and short title)
			 (reply (format nil "[ ~A ]::[ ~A ]" short title))
			 (reply (format nil "[ ~A ]::Couldn't fetch this page." url)))))))
	      ((and channel (not action))
	       (let ((trigger-tokens (triggered context token-text-list sender channel)))
		 (chain-in context token-text-list)
		 (when trigger-tokens
		   (let ((outgoing (chain context (first trigger-tokens) (second trigger-tokens))))
		     (unless (not (mismatch trigger-tokens outgoing :test #'string-equal))
		       (reply (format nil "~{~A~^ ~}" outgoing)))))))
	      (t nil))))))

(defun nye-hack ()
  (maphash-values
   #'(lambda (conn)
       (maphash-keys
	#'(lambda (channel)
	    (privmsg conn channel "Test test alkahest"))
        (channels conn)))
   *irc-connections*))

(defun say (utterance &key public channel channels)
  (cond
    (public (loop for cxn being the hash-values in *irc-connections* do
      (loop for k being the hash-keys in (channels cxn) do
	(privmsg cxn k utterance))))
    (channels (loop for cxn being the hash-values in *irc-connections* do
      (loop for k being the hash-keys in (channels cxn)
	    when (member k channels :test #'string-equal) do
	      (privmsg cxn k utterance))))
    (channel (loop for cxn being the hash-values in *irc-connections* do
      (loop for k being the hash-keys in (channels cxn)
	    when (string-equal k channel) do
	      (privmsg cxn k utterance))))))

#|
; Why do we fork another thread just to run this lambda, you may ask?
; Because the thread that the network event loop runs in keeps getting
; killed every time there's an error in any of this code, and then
; I have to restart the bot, and I get cranky.  That's why.
; This way, the thread that gets killed is an ephemeral thing that no-one
; (well, hardly anyone) will miss.
|#

(defun threaded-msg-hook (message)
  "Dispatch a thread to handle an incoming message."
  (make-thread #'(lambda ()
		   (msg-hook message nil))))

(defun threaded-action-hook (message)
  "Dispatch a thread to handle an incoming message."
  (make-thread #'(lambda ()
		   (msg-hook message t))))

(defun threaded-byebye-hook (message)
  "Handle a quit or part message."
  (make-thread #'(lambda ()
		   (let* ((connection (connection message))
			  (sender (source message)))
		     (setf (last-message connection) message)
		     (stop-ignoring connection sender)))))

(defun nick-change-hook (message)
  "Handle a nick message."
  (make-thread #'(lambda ()
		   (let* ((connection (connection message))
			  (sender (source message)))
		     (setf (last-message connection) message)
		     (stop-ignoring connection sender)))))

(defun stop-irc-client-instances ()
  "Shut down all the IRC connections."
    (maphash #'(lambda (k conn)
		 (let ((ircserver (first k))
		       (nickname (second k)))
		   (kill-bot-connection nickname ircserver)))
	     *irc-connections*))

(defun channel-member-list-on-join (message)
  (destructuring-bind
    (nick chan-visibility channel names)
      (arguments message)
    (declare (ignorable nick chan-visibility channel))
    (let* ((nl (split-sequence:split-sequence #\  names))
         (name-list (loop for name in nl
                          :collect (cl-ppcre:regex-replace-all "@|\\^|\&" name ""))))
    (values name-list))))

(defun notice-tracker (message)
  (format t "~&[NOTICE]:| ~A" message))

(defun irc-nick-change (message)
  (format t "~&[NICK CHANGE]:| ~A" (describe message))
  (let* ((connection (connection message)))))

(defun channel-rota (message)
  (format t "~&[ChannelROTA]| ~A~%~%" (describe message))
  (let* ((connection (connection message))
         (sender (source message)))
    (declare (ignorable connection))
    (format t "[ROTAx] ~A ~A" sender message)))

(defmethod cl-irc::default-hook :after ((message irc-rpl_namreply-message))
  "when the bot joins an irc channel, it catches a message of type
'irc-rpl_namreply-message (sic) which will catch this hook from the
messaging event loop as an :after method to the default combination.
It isn't totally clear to me why the cl-irc library establishes pretty
thorough generic function protocols around this eventing, and then
also exposes the literal #'add-hook functionality. A literal 'add-hook
hook runs before the default-hook, extended here."
  (when nil
    (let* ((connection (connection message)))
      (destructuring-bind
	  (nick chan-visibility channel names)
	  (arguments message)
	(declare (ignorable chan-visibility))
	(format t "~%~%~%CHANNEL JOIN DEFAULT HOOK, NAMES:: ~A~%~%~%" names)
	(let ((name-list (channel-member-list-on-join message)))
	  (loop for name in name-list
		:do (progn
		      (qmess connection channel (format nil "~&[HELLO!!] ~A from ~A~%" name nick))
		      )))))))

(defun make-irc-client-instance-thunk (nickname channels ircserver connection)
  "Make the thunk which moves in and instantiates a new IRC connection."
  (declare (ignorable nickname ircserver))
  (lambda ()
    (setf (bot-irc-client-thread connection) (bt:current-thread))
    (format t "~2&Channels: ~A" channels)

    (if (listp channels)
        (let ((ch (first channels))
              (pass (second channels)))
          (format t "||| Channel : ~A, Password: ~A |||~3%" ch pass)
          (cl-irc:join connection ch :password pass)
          (privmsg connection ch (format nil "NOTIFY:: Help, I'm a bot!")))
        (progn
          (format t "||| Channel (nopass) : ~A, Password: ~A |||~3%" channel nil)
          (cl-irc:join connection channel)
          (privmsg connection channel (format nil "NOTIFY:: Help, I'm a bot!"))))
    
    ;; (dolist (channel channels)
    ;;   (format t "~2&Type of 'channel: ~A~3%" (type-of channel))
    ;;   (format t "~&~S~3%" channel)
    ;;   )

    ;; backend processing. server joins, messages, user tracking
    (add-hook connection 'irc::irc-privmsg-message #'threaded-msg-hook)
    (add-hook connection 'irc::ctcp-action-message #'threaded-action-hook)
    (add-hook connection 'irc::irc-quit-message #'threaded-byebye-hook)
    (add-hook connection 'irc::irc-part-message #'threaded-byebye-hook)
    (add-hook connection 'irc::irc-notice-message #'notice-tracker)
    (add-hook connection 'irc::irc-nick-message #'irc-nick-change)
    ;; book-keeping hooks
    (add-hook connection 'irc::irc-rpl_namreply-message #'channel-rota)
    (read-message-loop connection)))

(defun make-bot-connection (nickname ircserver)
  "Make a connection to an IRC server."
  (let ((connection (connect :nickname nickname
			     :server ircserver
			     :connection-type 'bot-irc-connection)))
      (setf (gethash
	     (list (string-upcase ircserver)
		   (string-upcase nickname))
	     *irc-connections*) connection)
    connection))

(defun kill-bot-connection (nickname ircserver)
  (let* ((nickname (string-upcase nickname))
	 (ircserver (string-upcase ircserver))
	 (k (list ircserver nickname))
	 (connection (gethash k *irc-connections*)))
    (when connection
      (cl-irc:quit connection "I'm tired. I'm going home.")
      (stop-task (mq-task connection))
      (sleep 1)
      (bt:destroy-thread (bot-irc-client-thread connection))
      (remhash k *irc-connections*))))

(defun start-threaded-irc-client-instance (ircserver nickname channels)
  "Make a connection to an IRC server and spawn a thread to service it."
  (let ((connection (make-bot-connection nickname ircserver)))
    (make-thread
     (make-irc-client-instance-thunk nickname channels ircserver connection)
     :name (format nil "IRC Client thread: server ~A, nick ~A"
		   ircserver nickname))))

(defun start-threaded-irc-client-instances ()
  "Spawn a thread to run a session with an IRC server."
  (dolist (server-spec (irc-joins *bot-config*))
    (let ((ircserver (car server-spec)))
      (dolist (nick-spec (cadr server-spec))
	(let ((nickname (car nick-spec))
	      (channels (cadr nick-spec)))
	  (start-threaded-irc-client-instance ircserver nickname channels))))))

(defun print-bot-config (botconfig)
  "print the contents of the bot's configuration class."
  (dolist (server-spec (irc-joins botconfig))
    (let ((ircserver (car server-spec)))
      (dolist (nick-spec (cadr server-spec))
	(let ((nickname (car nick-spec))
	      (channels (cadr nick-spec)))
	  (format t "~&~{~A~^~%~}~%~%" (list ircserver nickname channels)))))))

(defun stop-threaded-irc-client-instances ()
  "Shut down a session with an IRC server, and clean up."
  (stop-irc-client-instances))
