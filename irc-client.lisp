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

(defmethod print-object ((object hash-table) stream)
  "give a reasonably informative printed representation of the contents
   of lisp hash-tables."
  (format stream "#HASH{~{~&~{(~a : ~a)~}~^~%~}}"
          (loop for key being the hash-keys of object
                  using (hash-value value)
                collect (list key value))))


;; We subclass cl-irc:connection and cl-irc:channel so we can store per-connection
;; and per-channel data here.

(defclass bot-irc-connection (cl-irc:connection)
  ((last-message :initform nil :accessor last-message)
   (bot-irc-client-thread :initform nil :accessor bot-irc-client-thread)
   (ignore-list :initform nil :accessor ignore-list) ;; vestigal, this slot isn't used.
   (message-q :initform (make-message-queue) :accessor message-q)
   ;;   (message-timer :initform nil :accessor message-timer)
   (mq-task :initform nil :accessor mq-task)))

(defclass bot-irc-channel (cl-irc:channel)
  ((trigger-list :initarg :trigger-list :initform nil :accessor trigger-list)
   (ignore-sticky :initarg :ignore-sticky
                  :initform (make-hash-table :test #'equalp :synchronized t) :accessor ignore-sticky)))

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
				 10 #'acceptable-word-p))
    (when (get-bot-channel-for-name channel-name (server-name connection))
      (log:debug "~2&|| Created channel entry for ~A ||~2%" channel-name))))

;; The rate-limiting queue structure for the connection to the IRC server.

(defparameter *mess-count* 0)

(defun make-mq-thunk (connection)
  "A closure to generate the function which gets called periodically to flush the queue."
  #'(lambda ()
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
    (log:debug "~&replying to: ~A~% with [~:D]: ~A~%" reply-to *mess-count* message)
    (privmsg connection reply-to message)))

(defmethod initialize-instance :after ((connection bot-irc-connection) &key)
  (start-irc-message-queue connection))

;; The !IGNOREME plumbing.

(defgeneric start-ignoring (connection sender channel)
  (:documentation "Begin ignoring sender on connection.  Returns nil if
sender was already being ignored, and a true value otherwise."))

(defgeneric stop-ignoring (connection sender channel)
  (:documentation "Stop ignoring sender on connection.  Returns nil if
sender wasn't being ignored; true otherwise."))

(define-condition database-fuckery (error)
  ())

(defun make-user-ignored (channel this-user channel/user-map)
  "Given a user, ignore the hell out of them, stickily."
  (declare (ignorable channel))

  (setf (ignored channel/user-map) t)
  (log:info "~&MAKE-USER-IGNORED:: THIS-USER IS: ~A" this-user)
  (handler-case
      (with-connection (psql-botdb-credentials *bot-config*)
        ;; (update-dao this-user)
        (update-dao channel/user-map))
    (error (c)
      (log:debug "Caught condition ~A in database update operation in MAKE-USER-IGNORED." c)
      nil)))

(defun make-user-unignored (channel this-user channel/user-map)
  "Given a user, listen intently, forever."
  ;; (declare (ignorable channel))
  
  (setf (ignored channel/user-map) nil)
  (log:info "~&MAKE-USER-UNIGNORED:: ~A / ~A~%" channel channel/user-map)
  (handler-case
      (with-connection (psql-botdb-credentials *bot-config*)
        (update-dao this-user)
        (update-dao channel/user-map))
    (error (c)
      (log:debug "~2&Caught condition ~A in database, unignoring ~A" c this-user))))


(defmethod start-ignoring ((connection bot-irc-connection) (sender string) (channel string))
  (log:info "~& #'START-IGNORING: CHANNEL: ~A ~A~%" (type-of channel) channel)
  (let* ((bot-irc-channel-name channel)

         (stored-state (gethash sender (ignore-sticky (gethash bot-irc-channel-name (channels connection))))))
    (log:debug "Super tired, this is a mess. ~A || ~A" bot-irc-channel-name stored-state)
    (with-channel-user bot-irc-channel-name  sender
      (if (eq channel/user-map stored-state)
          (log:debug "Too many!"))
      (if (not (ignored channel/user-map))
          (progn
            (log:info "~2&ignoring a fafo: ~A // ~A //~%" sender (current-handle this-user))
            ;; we change the database channel/user state in make-user-ignored
            (make-user-ignored this-channel this-user channel/user-map)
            ;; and then the cached value in the connection object of
            ;; the channel/user-map is a source of races in this code.
            (setf
             (gethash sender (ignore-sticky (gethash bot-irc-channel-name (channels connection))))
             channel/user-map)
            t)
          (progn
            (log:debug "~&START-IGNORING call on ignored user::CHAN: ~A~%THEUSERSTATE: ~A~%THIS-USER: ~A~%THIS-CHANNEL: ~A~%CHANNEL/USER-MAP: ~A~%" chan theuserstate this-user this-channel channel/user-map))))))

(defmethod stop-ignoring ((connection bot-irc-connection) sender channel)
  (with-channel-user channel sender
    ;; (log:debug "~&BLIXIT: CHAN: ~A~%THEUSERSTATE: ~A~%THIS-USER: ~A~%THIS-CHANNEL: ~A~%CHANNEL/USER-MAP: ~A~%" chan theuserstate this-user this-channel channel/user-map)
    (if (ignored channel/user-map)
        (progn
          (make-user-unignored this-channel this-user channel/user-map)
          (setf
           (gethash sender (ignore-sticky (gethash channel (channels connection))))
           channel/user-map)
          t)
        (progn
          (log:debug "~&STOP-IGNORING call on an unignored user:: CHAN: ~A~%THEUSERSTATE: ~A~%THIS-USER: ~A~%THIS-CHANNEL: ~A~%CHANNEL/USER-MAP: ~A~%" chan theuserstate this-user this-channel channel/user-map)))))

(defun ignoring-whom ()
  "Convenience function to print out who's on the global ignore list."
  (maphash #'(lambda (k v)
               ;; (format t "~2&[~A]~2%" v)
               (if (ignored (third v))
                   (log:debug "~&IGNORING: ~A on CHANNEL: ~A~%" k (channel-name (second v)))))
           *users*))

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
	     (log:debug "[~&in triggered] ~A~%" (trigger-list channel))
	     (setf (trigger-list channel) (append (trigger-list channel) token-list))
	     (log:debug "[~&after append token list in triggered] ~A~%" (trigger-list channel))
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

(defun find-the-bot-state (cname)
  "Given a channel name as a string, return the bot's 'BOT-IRC-CHANNEL
   protocol state"
  (let ((col (list)))
    (loop for cxn being the hash-values in *irc-connections*
          do
             (loop for k being the hash-keys in (channels cxn)
                     using (hash-value v)
                   do
                      (progn
                        (if (string= k cname)
                            (push v col)))))
    (if col
        (progn
          (log:debug "We have some ham: ~A~%" col)
          (first col))
        nil)))

(defun ignoring (connection sender text reply channel channel-name)
  "Apply commands related to ignoring/not ignoring the speaker.  Return t if
   the rest of the bot should ignore this utterance; return nil otherwise."

  (let ((ignore-phrase *ignore-phrase*))
    (cond ((string= ignore-phrase text)
           (log:debug "Caught BOT Ignore Phrase, ignoring cousin from across the river: ~A on channel: ~A"
                      sender channel)
	   (when (start-ignoring connection sender channel-name)
	     (privmsg connection sender ignore-phrase)))
          
	  ((string-equal text "!ignoreme off")
	   (if (stop-ignoring connection sender channel-name)
	       (funcall reply (format nil "No longer ignoring ~A." sender))
	       (funcall reply (format nil "I wasn't ignoring you, ~A." sender))))
          
	  ((scan (create-caseless-scanner "^!ignoreme") text)
	   (when (start-ignoring connection sender channel-name)
	     (funcall reply (format nil "Now ignoring ~A.  Use !IGNOREME OFF when you want me to hear you again." sender))))
          
	  ;; If there wasn't an ignore toggle command, look up the
	  ;; speaker's ignore status and return it. 
	  (t (cond (channel
                    (progn
                      ;; the ignore-sticky slot in the channel object
                      ;; contains a hash table, which should itself contain
                      ;; CHANNEL-USER objects, keyed by the username of the
                      ;; respective user.
                      (log:debug "~&HOON:: ~A ~2%" (gethash sender (ignore-sticky channel)))
                      (let* ((channel-user-ignored? (gethash sender (ignore-sticky channel))))
                        (if channel-user-ignored?
                            (return-from ignoring (ignored channel-user-ignored?))
                            (return-from ignoring nil))))
                    (t (return-from ignoring nil))))))
    ;; As there was an ignore toggle command, it's been handled and so should be ignored.
    nil))

(defun msg-hook (message action)
  "Handle an incoming message."
  (let* ((connection (connection message))
	 (channel-name (car (arguments message)))
         ;; the following becomes nil in the event of a query.
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
         ;; naively strip tracking components from pasted URLs.
	 (urls (mapcar #'de-utm-url (extract-urls text))))
    
    (flet ((irc-reply (s) (qmess connection reply-to s)))
      ;; who what where...
      (log:info "~&connection: ~A channel-name: ~A channel: ~A sender: ~A command: ~A context: ~A reply-to: ~A~%" connection channel-name channel sender command context reply-to)
      (setf (last-message connection) message)
      (log:info "Message: ~A~%" (raw-message-string message))
      (log:debug "MSG-HOOK flet/reply   connection=~A channel-name=~A~%" connection channel-name)
      ;; in following if channel is nil we're in a query.
      (unless (ignoring connection sender text #'irc-reply channel channel-name)
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
		   (when (scan "^www\." url)
		     (setf url (format nil "http://~A" url)))
		   (multiple-value-bind (short title) (lookup-url *the-url-store* context url sender)
		     (cond ((and short title)
                            (irc-reply (format nil "[ ~A ]::[ ~A ]" short title)))
                           ((not short)
                            (irc-reply (format nil "[ ~A ]::[ Couldn't fetch this page. ]" url)))
                           ((not title)
                            (irc-reply (format nil "[[ no title found for ~A ]]" url))
                            (log:debug "[[ no title found for ~A ]]" url)))))))
	      ((and channel (not action))
	       (let ((trigger-tokens (triggered context token-text-list sender channel)))
		 (chain-in context token-text-list)
		 (when trigger-tokens
		   (let ((outgoing (chain context (first trigger-tokens) (second trigger-tokens))))
		     (unless (not (mismatch trigger-tokens outgoing :test #'string-equal))
		       (irc-reply (format nil "~{~A~^ ~}" outgoing)))))))
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
                     (log:debug "~2&[WAT?!] really? ~A || ~A~2%" connection sender)
		     (setf (last-message connection) message)
		     ;; (stop-ignoring connection sender) ;; The user's ignore state should persist.
                     ))))

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
  (let* ((connection (connection message))
         (sender (source message))
         (text (regex-replace-all "\\ca" (second (arguments message)) "")))
    (log:debug "~2&[NOTICE]:| ~A ~A ~A" connection sender text)))

(defgeneric map-user-and-channel-to-sticky-state (botchannel userstring &key cumap &allow-other-keys)
  (:documentation "return the channel-user object recording the bot's disposition toward
a specific user in a specific channel."))

(defmethod map-user-and-channel-to-sticky-state ((botchannel bot-channel) (user string) &key (cumap nil))
  (let* ((channel-name (channel-name botchannel)))
    (if cumap
        cumap
        (get-channel-user-mapping (get-bot-channel-for-name channel-name)
                                       (get-user-for-handle user)))))

(defun user-join (message)
  "Handle channel join events, manage channel rota for persistent state (ignores)"
  (let* ((connection (connection message))
         (sender (source message))
         (channel-name (car (arguments message)))
         (channel (gethash channel-name (channels connection) )) ;; 'BOT-IRC-CHANNEL object
         (text (regex-replace-all "\\ca" (second (arguments message)) ""))
         (token-text-list (split "\\s+" text))
         (command (string-upcase (first token-text-list)))
         (cobject (get-bot-channel-for-name channel-name))
         (uobject (get-user-for-handle sender :channel channel-name))
         (channel/user-map (get-channel-user-mapping cobject uobject)))
    (declare (ignorable command channel))
    ;;=============================================================================================================
    ;;        connection                              sender   channel-name  channel text  token-text-list  command
    ;; #<BOT-IRC-CONNECTION irc.srh.org {1023CB5133}> SR-4     #trinity      NIL     NIL   NIL              NIL
    ;;=============================================================================================================
    (log:debug "~2&|USER-JOIN|>> ~A <<|USER-JOIN|~2%" channel/user-map)
    ;; (if (eq channel/user-map channel)
    ;;     (log:debug "~&|USER-STATE comparison ~A ~A" channel channel/user-map))
    (when channel/user-map
      (progn
        (log:debug "~2&[NEW USER OBJECT FOR USER JOIN] -> [ ~A ][ ~A ]" uobject (current-handle uobject))
        ;; (log:debug "~2& ~A~%" (describe uobject))

        ;; (setf (gethash (current-handle uobject) (ignore-sticky channel)) channel/user-map)

        (if (ignored channel/user-map)
            (progn
              (log:debug "~2& IGNORING USER: ~A~%" (current-handle uobject))
              (start-ignoring connection sender channel-name))
            (progn
              (log:debug "~2&(user-join ~A) Making new channel user: ~A~2%" message sender)
              (let* ((uobject (get-user-for-handle sender))
                     (cobject (make-bot-channel channel-name))
                     (cuo (get-channel-user-mapping cobject uobject))
                     (channel-users (if (listp cuo)
                                        (first cuo)
                                        cuo)))
                ;; (setf (gethash sender *users*) (list uobject cobject channel-users))
                (map-user-and-channel-to-sticky-state cobject (harlie-user-name uobject) :cumap channel-users))))))))

(defun irc-nick-change (message)
  (let* ((connection (connection message))
         (sender (source message))
         (channel-name (car (arguments message))) ;; in a nick change this is the new nick.
         (channel (gethash channel-name (channels connection) nil))
         (text (regex-replace-all "\\ca" (second (arguments message)) ""))
	 (token-text-list (split "\\s+" text))
	 (command (string-upcase (first token-text-list))))
    ;;=============================================================================================================
    ;;        connection                              sender   channel-name  channel text  token-text-list  command
    ;; #<BOT-IRC-CONNECTION irc.srh.org {1023CB5133}> SR-4     #trinity      NIL     NIL   NIL              NIL
    ;;=============================================================================================================

    (log:debug "~2&[NICK CHANGE ~A -> ~A] -- ~{~A~^ ~}~2%"
               sender channel-name
               (list connection sender channel-name channel text token-text-list command))
    
    ;; the nick is changing, so we need to update the channel-user
    ;; in the bot-irc-channel object appropriately.

    (with-channel-user channel-name sender
      (log:debug "~&[USER OBJECT FOR NICK CHANGE] -> ~A" (describe this-user))
      (setf (current-handle this-user) channel-name
            (prev-handle this-user) sender
            ;; (current-handle this-user) channel-name
            )
      (update-dao this-user))
    
    ;; (let* ((uobject (get-user-for-handle sender))) ;; get a new channel-user object from db
    ;;   (when uobject
    ;;     (log:debug "~2&[NEW USER OBJECT FOR NICK CHANGE] -> ~A" (describe uobject))
    ;;     (remhash sender *users*) ;; remove the previously cached object from *users*
    ;;     (setf (current-handle uobject) channel-name) ;; update the current-handle slot with the new name
    ;;     (setf (prev-handle uobject) sender) ;; set the prev-handle slot to the previous handle
    ;;     (with-connection (psql-botdb-credentials *bot-config*)
    ;;       (update-dao uobject)) ;; save the updated info in the database
    ;;     (setf (current-handle uobject) *users*)))
    )) ;; save the updated object to the cache in *users*


(defgeneric get-channel-object-from-connection (connection cname)
  (:documentation "The bot partitions who it knows about by channel, which are objects
with slots for various states that we care about, including the
triggerlist and the persistent ignore metadata. We need a means to
access this object when we aren't in the throes of a message event."))

(defmethod get-channel-object-from-connection ((connection bot-irc-connection) (cname string))
  (gethash cname (channels connection)))

(defmethod cl-irc::default-hook :after ((message irc-rpl_namreply-message))
  "when the bot joins an irc channel, it catches a message of type
'IRC-RPL_NAMREPLY-MESSAGE (sic) which will snag this hook from the
messaging event loop as an :after method to the default combination.
It isn't totally clear to me why the cl-irc library establishes pretty
thorough generic function protocols around this eventing, and then
also exposes the literal #'add-hook functionality. A literal #'add-hook
hook runs before the default-hook, extended here."
  (let* ((connection (connection message)))
    (destructuring-bind
	(nick chan-visibility channel names)
	(arguments message)
      (declare (ignorable chan-visibility ))

      (let* ((chan-obj-hash (get-channel-object-from-connection connection channel)))
        (log:debug "~4&This is your channel object hash: ~A. There are many like it, but this one is yours.~2%" chan-obj-hash)
        ;;  (log:debug "~2&[[[~{ ~A~^| ~}]]]~2%" (list nick chan-visibility channel names))
        (log:debug "~2&CHANNEL:: ~A JOIN DEFAULT HOOK, NAMES:: ~A~2%" channel names)

        (let ((name-list (channel-member-list-on-join message))) ;; seems redundant but this cleans punctuation
          (loop for name in name-list
                :do (progn
                      (when (not (string= nick name)) ;; don't act on ourself
                        ;; establish user objects.
                        ;; print the name of the user, and the enclosing channel.
                        (log:info "~&->->-> Acting for user: ~A on channel: ~A~%" name channel)
                        (with-channel-user channel name
                          (let ((this-user-name (harlie-user-name this-user)))
                            (if this-user
                                (progn
                                  (log:debug "~2&Adding user ~A to channel ~A~2%"
                                             (current-handle this-user)
                                             (channel-name this-channel))
                                  ;; set us up all the state.
                                  ;; set the channel/user-map state in the BOT-IRC-CHANNEL object
                                  (setf                                   
                                   (gethash this-user-name (ignore-sticky chan-obj-hash)) channel/user-map)
                                  ;; if the name is ignored in the database, ignore it in the world
                                  (when (ignored channel/user-map)
                                    (start-ignoring connection this-user-name channel))))))))))))))

(defun make-irc-client-instance-thunk (nickname channels ircserver connection)
  "Make the thunk which moves in and instantiates a new IRC connection."
  (declare (ignorable nickname ircserver))
  (lambda ()
    (setf (bot-irc-client-thread connection) (bt:current-thread))
    (log:debug "~2&Channels: ~A" channels)

    (if (listp channels)
        (let ((ch (first channels))
              (pass (second channels)))
          (log:debug "||| Channel : ~A, Password: ~A |||~3%" ch pass)
          (cl-irc:join connection ch :password pass)
          (privmsg connection ch (format nil "NOTIFY:: Help, I'm a bot!")))
        (progn
          (log:debug "||| Channel (nopass) : ~A, Password: ~A |||~3%" channel nil)
          (cl-irc:join connection channel)
          (privmsg connection channel (format nil "NOTIFY:: Help, I'm a bot!"))))
    
    ;; (dolist (channel channels)
    ;;   (log:debug "~2&Type of 'channel: ~A~3%" (type-of channel))
    ;;   (log:debug "~&~S~3%" channel)
    ;;   )

    ;; Protocol processing. server joins, messages, user tracking
    (add-hook connection 'irc::irc-privmsg-message #'threaded-msg-hook)
    (add-hook connection 'irc::ctcp-action-message #'threaded-action-hook)
    (add-hook connection 'irc::irc-quit-message #'threaded-byebye-hook)
    (add-hook connection 'irc::irc-part-message #'threaded-byebye-hook)
    (add-hook connection 'irc::irc-notice-message #'notice-tracker)
    (add-hook connection 'irc::irc-join-message #'user-join)
    (add-hook connection 'irc::irc-nick-message #'irc-nick-change)
    ;; book-keeping hooks
    (add-hook connection 'irc::irc-rpl_namreply-message #'channel-member-list-on-join)
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

(defun start-threaded-irc-client-instances (&key (go? nil))
  "Spawn a thread to run a session with an IRC server."
  (dolist (server-spec (irc-joins *bot-config*))
    (let ((ircserver (car server-spec)))
      (log:debug "~&IRC Server for joins: ~A" ircserver)
      (dolist (nick-spec (cadr server-spec))
        ;; (log:debug "~&[[[ how is this even?? ~A ]]]" nick-spec)
	(let ((nickname (car nick-spec))
	      (channels (cadr nick-spec)))
          (log:debug "IRC handle: ~A on channel:~A" nickname channels)
          (when go?
            (start-threaded-irc-client-instance ircserver nickname channels)))))))

(defun print-bot-config (botconfig)
  "print the contents of the bot's configuration class."
  (dolist (server-spec (irc-joins botconfig))
    (let ((ircserver (car server-spec)))
      (dolist (nick-spec (cadr server-spec))
	(let ((nickname (car nick-spec))
	      (channels (cadr nick-spec)))
	  (log:debug "~&~{~A~^~%~}~%~%" (list ircserver nickname channels)))))))

(defun stop-threaded-irc-client-instances ()
  "Shut down a session with an IRC server, and clean up."
  (stop-irc-client-instances))
