;;;; irc-client.lisp

(in-package #:harlie)

(declaim (optimize (debug 3) (safety 3) (speed 0)))

(defvar *irc-connections* (make-hash-table :test 'equal :synchronized t))

(defun some-connection ()
  "Sometimes you just want to grab a connection object from the REPL."
  (car (loop for conn being the hash-values in *irc-connections* collecting conn)))

(defun troubleshoot-queues ()
  (mapc #'(lambda (x)
	    (format t "~A ~A~%" x (sb-ext:timer-scheduled-p (message-timer (gethash x *irc-connections*))))
	    (format t "~{~A~^~%~}" (sb-concurrency:list-queue-contents (message-q (gethash x *irc-connections*))))
	    )
	(loop for conn-key being the hash-keys in *irc-connections* collecting conn-key))
  nil)

;; We subclass cl-irc:connection and cl-irc:channel so we can store per-connection
;; and per-channel data here.

(defclass bot-irc-connection (cl-irc:connection)
  ((last-message :initform nil :accessor last-message)
   (bot-irc-client-thread :initform nil :accessor bot-irc-client-thread)
   (ignore-list :initform nil :accessor ignore-list)
   (message-q :initform (make-queue :name (format nil "IRC message queue")) :accessor message-q)
   (message-timer :initform nil :accessor message-timer)))

(defclass bot-irc-channel (cl-irc:channel)
  ((trigger-list :initarg :trigger-list :initform nil :accessor trigger-list)))

;; A couple of bodges which force cl-irc to instantiate our bot-irc-channel objects
;; instead of its channel objects.
;;
;; Our bot-irc-connection class gets used because we can pass a symbol for the
;; class in to the connect function, which we do in start-threaded-irc-client-instance
;; below.  But I can't see a simple way to override the creation of channels to
;; use our subclass for those instead.
;;
;; intercept-join-message is registered as a hook which gets called upon receipt
;; of join message from the server.  It's a copy of the corresponding
;; callback from cl-irc, except that we call make-rigged-channel instead of
;; make-channel.
;;
;; make-rigged-channel is likewise a copy of make-channel except that it instantiates
;; a bot-irc-channel object instead of a channel object.
;;
;; Oh, and we have to qualify all kinds of symbols here as being from cl-irc.

(defun make-rigged-channel (connection
			    &key (name "")
			      (topic "")
			      (modes nil)
			      (users nil)
			      (user-count nil))
  "Half of the hook to ram our bot-irc-channel type into the cl-irc amalgam."
  (let ((channel
	  (make-instance 'bot-irc-channel
			 :name name
			 :normalized-name
			 (cl-irc:normalize-channel-name connection name)
			 :topic topic
			 :modes modes
			 :user-count user-count
			 :trigger-list (random-words
					(make-instance 'bot-context
						       :bot-nick (nickname (user connection))
						       :bot-irc-server (server-name connection)
						       :bot-irc-channel name)
					10 #'acceptable-word-p))))
    (dolist (user users)
      (cl-irc:add-user channel user))
    channel))

(defun intercept-join-message (message)
  "The other half of the hook for insinuating bot-irc-channel into cl-irc."
  (with-slots
	(cl-irc:connection cl-irc:source cl-irc:host cl-irc:user cl-irc:arguments)
      message
    (destructuring-bind
        (channel)
        arguments
      (let ((user (cl-irc::find-or-make-user cl-irc:connection cl-irc:source
					     :hostname cl-irc:host
					     :username cl-irc:user))
	    (channel (or (cl-irc:find-channel cl-irc:connection channel)
                         (make-rigged-channel connection :name channel))))
        (when (cl-irc:self-message-p message)
          (cl-irc:add-channel cl-irc:connection channel))
        (cl-irc:add-user cl-irc:connection user)
        (cl-irc:add-user channel user)))))

;; The rate-limiting queue structure for the connection to the IRC server.

(defparameter *mess-count* 0)

(defun make-q-runner (connection)
  "A closure to generate the function which gets called periodically to flush the queue."
  #'(lambda ()
      (when (not (queue-empty-p (message-q connection)))
	(dqmess connection))
      ;; (format t "Rescheduling the q runner...")
      (sb-ext:schedule-timer (message-timer connection) (max 1.5 (random 2.5)))
      ;; (format t " [Done :: ~A]~&" (get-universal-time))
      ))

(defun start-irc-message-queue (connection)
  "A convenience function to regenerate the queue flusher and restart it."
  (let ((q-runner (make-q-runner connection)))
    (setf (message-timer connection)
	  (sb-ext:make-timer q-runner :name "queue runner." :thread t))
    (funcall q-runner)))

(defgeneric qmess (connection reply-to message)
  (:documentation "queue a message for rate-limited sending."))

(defgeneric dqmess (connection)
  (:documentation "dequeue a message to send."))

(defmethod qmess ((connection bot-irc-connection) reply-to message)
  (let* ((count (incf *mess-count*))) ;; (message (format nil "[~:D] ~A" count message))
    (format t "~&Queuing message [~:D]::~% ~A" count message)
    (enqueue (list reply-to message) (message-q connection))))

(defmethod dqmess ((connection bot-irc-connection))
  (let* ((mobj (dequeue (message-q connection)))
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
  (sb-ext:with-locked-hash-table (*irc-connections*)
    (loop for k being the hash-keys
	  in *irc-connections*
	  append (list k (ignore-list
			  (gethash k *irc-connections*))))))

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

(defun msg-hook (message)
  "Handle an incoming message."
  (let* ((connection (connection message))
	 (channel-name (car (arguments message)))
	 (channel (sb-ext:with-locked-hash-table
		      ((channels connection))
		    (gethash channel-name (channels connection) nil)))
	 (sender (source message))
	 (text (regex-replace-all "\\ca" (second (arguments message)) ""))
	 (token-text-list (split "\\s+" text))
	 (command (string-upcase (first token-text-list)))
	 (reply-to channel-name)
	 (context (make-instance
		   'bot-context
		   :bot-nick (nickname (user connection))
		   :bot-irc-server (server-name connection)
		   :bot-irc-channel channel))
	 (urls (extract-urls text)))

    (setf (last-message connection) message)
    (format t "Message: ~A~%" (raw-message-string message))
    (format t "   connection=~A channel=~A~%" connection channel-name)

    (when (string-equal channel-name (bot-nick context))
      (setf reply-to sender))

    (cond ((scan "^NOTIFY:: Help, I'm a bot!" text)
	   (when (start-ignoring connection sender)
	     (privmsg connection sender "NOTIFY:: Help, I'm a bot!")))

	  ((string= "!IGNOREME" command)
	   (if (or
		(< (length token-text-list) 2)
		(not (string= "OFF" (string-upcase (second token-text-list)))))
	       (when (start-ignoring connection sender)
		 (qmess connection reply-to
			(format nil
				"Now ignoring ~A.  Use !IGNOREME OFF when you want me to hear you again." sender)))
	       (if (stop-ignoring connection sender)
		   (qmess connection reply-to
			  (format nil "No longer ignoring ~A." sender))
		   (qmess connection reply-to
			  (format nil "I wasn't ignoring you, ~A." sender)))))
	  
	  ((member sender (ignore-list connection) :test #'string-equal) nil)

	  ((scan "^![^!]" command)
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
	       (multiple-value-bind (short title tweet) (lookup-url *the-url-store* context url sender)
		 (if (and short title)
		     (progn
		       (qmess connection reply-to
			      (format nil "[ ~A ] [ ~A ]" short title))
		       (when tweet
			 (qmess connection reply-to
				(format nil "~A" tweet))))
		     (qmess connection reply-to
			    (format nil "[ ~A ] Couldn't fetch this page." url)))))))

	  (channel
	   (let ((trigger-tokens (triggered context token-text-list sender channel)))
	     (chain-in context token-text-list)
	     (when trigger-tokens
	       (let ((outgoing (chain context (first trigger-tokens) (second trigger-tokens))))
		 (unless (not (mismatch trigger-tokens outgoing :test #'string-equal))
		   (qmess connection reply-to
			  (format nil "~{~A~^ ~}" outgoing)))))))
	  (t nil))))

(defun nye-hack ()
  (mapc #'(lambda (conn)
	    (loop for k being the hash-keys in (channels conn) do (privmsg conn k "Test test alkahest")))
	(loop for conn being the hash-values in *irc-connections* collecting conn)))

; Why do we fork another thread just to run this lambda, you may ask?
; Because the thread that the network event loop runs in keeps getting
; killed every time there's an error in any of this code, and then
; I have to restart the bot, and I get cranky.  That's why.
; This way, the thread that gets killed is an ephemeral thing that no-one
; (well, hardly anyone) will miss.

(defun threaded-msg-hook (message)
  "Dispatch a thread to handle an incoming message."
  (make-thread #'(lambda ()
		   (msg-hook message))))

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

(defun stop-irc-client-instance ()
  "Shut down a session with the IRC server."
  (sb-ext:with-locked-hash-table (*irc-connections*)
    (loop for k being the hash-keys in *irc-connections*
	  do (let ((connection (sb-ext:with-locked-hash-table (*irc-connections*)
				 (gethash k *irc-connections*))))
	       (cl-irc:quit connection  "I'm tired. I'm going home.")
	       (sb-ext:unschedule-timer (message-timer connection))
	       (sleep 1)
	       (bt:destroy-thread (bot-irc-client-thread connection))
	       (remhash k *irc-connections*)))))

(defun start-threaded-irc-client-instance ()
  "Spawn a thread to run a session with an IRC server."
  (dolist (nickchans (irc-nickchannels *bot-config*))
    (let* ((nickname (car nickchans))
	   (channels (cadr nickchans))
	   (ircserver (irc-server-name *bot-config*))
	   (connection (connect :nickname nickname
				:server ircserver
				:connection-type 'bot-irc-connection)))
      (make-thread
       #'(lambda ()
	   (setf (bot-irc-client-thread connection) (bt:current-thread))
	   (sb-ext:with-locked-hash-table (*irc-connections*)
	     (setf (gethash
		    (list (string-upcase ircserver)
			  (string-upcase nickname))
		    *irc-connections*) connection))
	   (dolist (channel channels)
	     (if (listp channel)
		 (progn
		   (cl-irc:join connection (first channel) :password (second channel))
		   (privmsg connection (first channel) (format nil "NOTIFY:: Help, I'm a bot!")))
		 (progn
		   (cl-irc:join connection channel)
		   (privmsg connection channel (format nil "NOTIFY:: Help, I'm a bot!")))))
	   (add-hook connection 'irc::irc-privmsg-message #'threaded-msg-hook)
	   (add-hook connection 'irc::ctcp-action-message #'threaded-msg-hook)
	   (add-hook connection 'irc::irc-quit-message #'threaded-byebye-hook)
	   (add-hook connection 'irc::irc-part-message #'threaded-byebye-hook)
	   (add-hook connection 'irc::irc-join-message #'intercept-join-message)
	   (read-message-loop connection))
       :name (format nil "IRC Client thread: server ~A, nick ~A"
		     ircserver nickname)))))

(defun stop-threaded-irc-client-instance ()
  "Shut down a session with an IRC server, and clean up."
  (stop-irc-client-instance))
