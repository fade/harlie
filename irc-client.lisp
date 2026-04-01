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

;; We subclass clatter-irc:connection so we can store per-connection data here.
;; NOTE: clatter-irc already provides: connection-nick, connection-server,
;; connection-port, connection-state (internal), auto-reconnect, TLS, SASL,
;; thread-safe send-raw, built-in read loop, and hook system.

(defclass bot-irc-connection (connection)
  ((last-message :initform nil :accessor last-message)
   (bot-irc-client-thread :initform nil :accessor bot-irc-client-thread)
   (ignore-list :initform nil :accessor ignore-list) ;; vestigal, this slot isn't used.
   (message-q :initform (make-message-queue) :accessor message-q)
   ;;   (message-timer :initform nil :accessor message-timer)
   (mq-task :initform nil :accessor mq-task)
   (bot-channels :initform (make-hash-table :test 'equalp :synchronized t) :accessor channels
                 :documentation "Hash of channel-name -> bot-irc-channel objects")
   (connection-state :initform :connecting :accessor connection-state
                     :documentation "Tracks registration lifecycle:
:connecting -> :registered -> :identifying -> :joining -> :joined | :join-failed
                              -> :registering --^  (when nick unregistered and email provided)")
   (nickserv-password :initform nil :accessor nickserv-password
                      :documentation "NickServ password for IDENTIFY/REGISTER, or nil if not needed.")
   (nickserv-email    :initform nil :accessor nickserv-email
                      :documentation "Email address for NickServ REGISTER. nil if not registering.")))

;; bot-irc-channel is now a standalone class (no longer subclassing cl-irc:channel).
;; It stores per-channel state: trigger words and per-user ignore status.

(defclass bot-irc-channel ()
  ((channel-name :initarg :channel-name :initform nil :accessor bot-channel-name)
   (trigger-list :initarg :trigger-list :initform nil :accessor trigger-list)
   (ignore-sticky :initarg :ignore-sticky
                  :initform (make-hash-table :test #'equalp :synchronized t) :accessor ignore-sticky)))

;; When the bot joins a channel, create a bot-irc-channel and populate the trigger list.
;; This is wired up as a clatter-irc on-join hook in make-irc-client-instance-thunk.

(defun setup-bot-channel (conn nick channel-name)
  "Create a bot-irc-channel entry for CHANNEL-NAME in CONN's channel hash."
  (handler-case
      (let ((bot-chan (make-instance 'bot-irc-channel
                                     :channel-name channel-name
                                     :trigger-list (random-words
                                                    (make-instance 'bot-context
                                                                   :bot-nick (connection-nick conn)
                                                                   :bot-irc-server (connection-server conn)
                                                                   :bot-irc-channel channel-name)
                                                    10 #'acceptable-word-p))))
        (setf (gethash channel-name (channels conn)) bot-chan)
        (when (get-bot-channel-for-name channel-name (connection-server conn))
          (log:debug "~2&|| Created channel entry for ~A ||~2%" channel-name)))
    (error (e)
      (log:warn "~&[JOIN HOOK] Error setting up channel ~A: ~A"
                channel-name e))))

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
      (with-connection (db-credentials *bot-config*)
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
      (with-connection (db-credentials *bot-config*)
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

(defun msg-hook (conn msg sender channel-name text action)
  "Handle an incoming message.
   CONN is the bot-irc-connection, MSG is the clatter-irc message object,
   SENDER is the nick, CHANNEL-NAME is the target, TEXT is the message text,
   ACTION is t for CTCP ACTION, nil for regular PRIVMSG."
  (let* ((channel (gethash channel-name (channels conn) nil))
         ;; the following becomes nil in the event of an irc query (/msg).
	 (text (regex-replace-all "\\ca" text ""))
         (token-text-list (split "\\s+" text))
	 (command (string-upcase (first token-text-list)))
	 (context (make-instance
		   'bot-context
		   :bot-nick (connection-nick conn)
		   :bot-irc-server (connection-server conn)
		   :bot-irc-channel channel))
	 (reply-to (if (string-equal channel-name (bot-nick context))
		       sender
		       channel-name))
         ;; naively strip tracking components from pasted URLs.
	 (urls (mapcar #'de-utm-url (extract-urls text))))
    
    (flet ((irc-reply (s) (qmess conn reply-to s)))
      ;; who what where...
      (log:info "~&connection: ~A channel-name: ~A channel: ~A sender: ~A command: ~A context: ~A reply-to: ~A~%" conn channel-name channel sender command context reply-to)
      (setf (last-message conn) msg)
      (log:info "Message: ~A~%" (message-raw msg))
      (log:debug "MSG-HOOK flet/reply   connection=~A channel-name=~A~%" conn channel-name)
      ;; in following if channel is nil we're in a query.
      (unless (ignoring conn sender text #'irc-reply channel channel-name)
	(cond ((scan "^![^!]" command)
	       (run-plugin (make-instance
			    'plugin-request :botcmd command
					    :plugin-context context
					    :connection conn
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

(defun threaded-msg-hook (conn msg sender target text)
  "Dispatch a thread to handle an incoming PRIVMSG.
   Hook signature for clatter-irc on-privmsg: (conn msg sender target text)."
  (make-thread #'(lambda ()
		   (msg-hook conn msg sender target text nil))))

(defun threaded-action-hook (conn msg sender target ctcp-cmd ctcp-args)
  "Dispatch a thread to handle a CTCP ACTION as a message.
   Hook signature for clatter-irc on-ctcp: (conn msg sender target ctcp-cmd ctcp-args)."
  (when (string-equal ctcp-cmd "ACTION")
    (make-thread #'(lambda ()
		     (msg-hook conn msg sender target ctcp-args t)))))

(defun threaded-byebye-hook (conn msg nick &rest rest)
  "Handle a quit or part message.
   Works for both on-quit (conn msg nick reason) and on-part (conn msg nick channel reason)."
  (declare (ignore rest))
  (make-thread #'(lambda ()
                   (log:debug "~2&[WAT?!] really? ~A || ~A~2%" conn nick)
		   (setf (last-message conn) msg))))

;; (defun nick-change-hook (message)
;;   "Handle a nick message."
;;   (make-thread #'(lambda ()
;; 		   (let* ((connection (connection message))
;; 			  (sender (source message)))
;; 		     (setf (last-message connection) message)
;; 		     (stop-ignoring connection sender)))))

(defun stop-irc-client-instances ()
  "Shut down all the IRC connections."
  (maphash #'(lambda (k conn)
               (let ((ircserver (first k))
                     (nickname (second k)))
                 (kill-bot-connection nickname ircserver)))
           *irc-connections*))

(defun channel-member-list-on-join (params)
  "Parse a 353 RPL_NAMREPLY params list into a cleaned name list."
  (destructuring-bind
    (nick chan-visibility channel names)
      params
    (declare (ignorable nick chan-visibility channel))
    (let* ((nl (split-sequence:split-sequence #\  names))
         (name-list (loop for name in nl
                          :collect (cl-ppcre:regex-replace-all "@|\\^|\&" name ""))))
    (values name-list))))

(defun notice-tracker (conn msg sender target text)
  "Track NOTICE messages.
   Hook signature for clatter-irc on-notice: (conn msg sender target text)."
  (declare (ignore msg target))
  (log:debug "~2&[NOTICE]:| ~A ~A ~A" conn sender text))

(defgeneric map-user-and-channel-to-sticky-state (botchannel userstring &key cumap &allow-other-keys)
  (:documentation "return the channel-user object recording the bot's disposition toward
a specific user in a specific channel."))

(defmethod map-user-and-channel-to-sticky-state ((botchannel bot-channel) (user string) &key (cumap nil))
  (let* ((channel-name (channel-name botchannel)))
    (if cumap
        cumap
        (get-channel-user-mapping (get-bot-channel-for-name channel-name)
                                       (get-user-for-handle user)))))

(defun user-join (conn msg joining-nick channel-name)
  "Handle channel join events, manage channel rota for persistent state (ignores).
   Hook signature for clatter-irc on-join: (conn msg nick channel)."
  (declare (ignore msg))
  (handler-case
   (let* ((channel (gethash channel-name (channels conn))) ;; 'BOT-IRC-CHANNEL object
          (cobject (get-bot-channel-for-name channel-name))
          (uobject (get-user-for-handle joining-nick :channel channel-name))
          (channel/user-map (get-channel-user-mapping cobject uobject)))
    (declare (ignorable channel))
    (log:debug "~2&|USER-JOIN|>> ~A <<|USER-JOIN|~2%" channel/user-map)
    (when channel/user-map
      (progn
        (log:debug "~2&[NEW USER OBJECT FOR USER JOIN] -> [ ~A ][ ~A ]" uobject (current-handle uobject))

        (if (ignored channel/user-map)
            (progn
              (log:debug "~2& IGNORING USER: ~A~%" (current-handle uobject))
              (start-ignoring conn joining-nick channel-name))
            (progn
              (log:debug "~2&(user-join ~A) Making new channel user: ~A~2%" joining-nick joining-nick)
              (let* ((uobject (get-user-for-handle joining-nick))
                     (cobject (make-bot-channel channel-name))
                     (cuo (get-channel-user-mapping cobject uobject))
                     (channel-users (if (listp cuo)
                                        (first cuo)
                                        cuo)))
                (map-user-and-channel-to-sticky-state cobject (harlie-user-name uobject) :cumap channel-users))))))
   (error (e)
     (log:warn "~&[USER-JOIN] Error handling join for ~A: ~A"
               joining-nick e)))))

(defun irc-nick-change (conn msg old-nick new-nick)
  "Handle a nick change event.
   Hook signature for clatter-irc on-nick: (conn msg old-nick new-nick)."
  (declare (ignore msg conn))
    
  (log:info "NEW NICK: ~A" new-nick)
    
  ;; the nick is changing, so we need to update the channel-user
  ;; in the bot-irc-channel object appropriately.

  (with-handle-swap old-nick new-nick
    (log:debug "~2&[NICK CHANGE ~A -> ~A]~2%" old-nick new-nick)
    (setf (current-handle this-user) new-nick
          (prev-handle this-user) old-nick
          (last-seen this-user) (local-time:now))
    (with-connection (db-credentials *bot-config*)
      (update-dao this-user))))

;; (defun handle-swap (old new)
;;   (with-handle-swap new old
;;     (log:debug "~&[USER OBJECT FOR NICK CHANGE] -> ~A" (describe this-user))
;;     ;; (log:debug "~2&[NICK CHANGE ~A -> ~A] -- ~{~A~^ ~}~2%"
;;     ;;            sender channel-name
;;     ;;            (list connection sender channel-name channel text token-text-list command))
;;     (setf (current-handle this-user) new-nick
;;           (prev-handle this-user) old-nick)
;;     (with-connection (db-credentials *bot-config*)
;;       (update-dao this-user))))


(defgeneric get-channel-object-from-connection (connection cname)
  (:documentation "The bot partitions who it knows about by channel, which are objects
with slots for various states that we care about, including the
triggerlist and the persistent ignore metadata. We need a means to
access this object when we aren't in the throes of a message event."))

(defmethod get-channel-object-from-connection ((connection bot-irc-connection) (cname string))
  (gethash cname (channels connection)))

(defun handle-namreply (conn msg)
  "Handle RPL_NAMREPLY (353) — populate user state when the bot joins a channel.
   Called from the on-numeric hook when code = 353."
  (let ((params (message-params msg)))
    (when (>= (length params) 4)
      (destructuring-bind
          (nick chan-visibility channel names)
          params
        (declare (ignorable chan-visibility))

        (let* ((chan-obj-hash (get-channel-object-from-connection conn channel)))
          (log:debug "~4&This is your channel object hash: ~A. There are many like it, but this one is yours.~2%" chan-obj-hash)
          (log:debug "~2&CHANNEL:: ~A JOIN DEFAULT HOOK, NAMES:: ~A~2%" channel names)

          (let ((name-list (channel-member-list-on-join params)))
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
                                      (start-ignoring conn this-user-name channel)))))))))))))

(defun make-irc-client-instance-thunk (nickname channel channel-key ircserver connection)
  "Make the thunk which sets up hooks and connects to IRC via clatter-irc.

   CHANNEL and CHANNEL-KEY are plain strings (or nil for no key).
   Channel join is deferred until 001 RPL_WELCOME confirms registration.
   If NICKSERV-PASSWORD is set on the connection, IDENTIFY is sent first and
   the JOIN is deferred further until NickServ acknowledges identification.

   clatter-irc handles the read loop, reconnection, and PING/PONG internally."
  (declare (ignorable ircserver))
  (let ((channel-password channel-key))
    (lambda ()
      (setf (bot-irc-client-thread connection) (bt:current-thread))
      (setf (connection-state connection) :connecting)
      (log:info "~2&[CONNECT] nick=~A channel=~A server=~A" nickname channel ircserver)

      ;; --- on-connect: fired after 001 RPL_WELCOME ---
      (add-hook connection 'on-connect
                (lambda (conn)
                  (setf (connection-state conn) :registered)
                  (log:info "~&[001] Registered as ~A on ~A" nickname ircserver)
                  (if (nickserv-password conn)
                      (progn
                        (setf (connection-state conn) :identifying)
                        (log:info "~&[NickServ] Sending IDENTIFY for ~A" nickname)
                        (privmsg conn "NickServ"
                                 (format nil "IDENTIFY ~A" (nickserv-password conn))))
                      (progn
                        (setf (connection-state conn) :joining)
                        (log:info "~&[JOIN] ~A joining ~A on ~A" nickname channel ircserver)
                        (join conn channel channel-password)))))

      ;; --- NickServ NOTICE: identification, registration, and join sequencing ---
      (add-hook connection 'on-notice
                (lambda (conn msg sender target text)
                  (declare (ignore msg target))
                  (when (string-equal sender "NickServ")
                    (cond
                      ;; Identification confirmed → join.
                      ((and (eq (connection-state conn) :identifying)
                            (or (search "You are now identified" text)
                                (search "You are now logged in" text)))
                       (setf (connection-state conn) :joining)
                       (log:info "~&[NickServ] Identified. ~A joining ~A on ~A"
                                 nickname channel ircserver)
                       (join conn channel channel-password))

                      ;; Nick not registered → REGISTER if we have an email, else just join.
                      ((and (eq (connection-state conn) :identifying)
                            (search "not registered" text))
                       (if (nickserv-email conn)
                           (progn
                             (setf (connection-state conn) :registering)
                             (log:info "~&[NickServ] ~A not registered. Sending REGISTER to ~A."
                                       nickname (nickserv-email conn))
                             (privmsg conn "NickServ"
                                      (format nil "REGISTER ~A ~A"
                                              (nickserv-password conn)
                                              (nickserv-email conn))))
                           (progn
                             (log:warn "~&[NickServ] ~A not registered and no email configured. Joining without identification."
                                       nickname)
                             (setf (connection-state conn) :joining)
                             (join conn channel channel-password))))

                      ;; Registration response received → log it and proceed to join.
                      ;; The nick will be unverified until the user clicks the email link;
                      ;; on the next restart IDENTIFY will work normally.
                      ((eq (connection-state conn) :registering)
                       (log:info "~&[NickServ] Registration response for ~A: ~A"
                                 nickname text)
                       (when (or (search "email" text)
                                 (search "registered" text)
                                 (search "activation" text))
                         (log:info "~&[NickServ] ~A registered. Check ~A for activation link. Joining now."
                                   nickname (nickserv-email conn))
                         (setf (connection-state conn) :joining)
                         (join conn channel channel-password)))))))

      ;; --- on-join: bot channel setup and announcement on our own join ---
      (add-hook connection 'on-join
                (lambda (conn msg joining-nick joined-channel)
                  (declare (ignore msg))
                  ;; Set up bot-irc-channel object for any join
                  (setup-bot-channel conn (connection-nick conn) joined-channel)
                  ;; Our own join: announce and update state
                  (when (string-equal joining-nick nickname)
                    (setf (connection-state conn) :joined)
                    (log:info "~&[JOINED] ~A is now in ~A on ~A"
                              nickname joined-channel ircserver)
                    (privmsg conn joined-channel *ignore-phrase*))
                  ;; Handle user join tracking (for non-self joins)
                  (unless (string-equal joining-nick nickname)
                    (user-join conn nil joining-nick joined-channel))))

      ;; --- on-numeric: join errors and RPL_NAMREPLY ---
      (add-hook connection 'on-numeric
                (lambda (conn msg code name)
                  (declare (ignore name))
                  (case code
                    ;; JOIN error codes
                    (403 (log:warn "~&[JOIN FAIL 403 no-such-channel] ~A ~A"
                                   channel (message-params msg))
                         (setf (connection-state conn) :join-failed))
                    (405 (log:warn "~&[JOIN FAIL 405 too-many-channels] ~A ~A"
                                   channel (message-params msg))
                         (setf (connection-state conn) :join-failed))
                    (473 (log:warn "~&[JOIN FAIL 473 invite-only] ~A ~A"
                                   channel (message-params msg))
                         (setf (connection-state conn) :join-failed))
                    (474 (log:warn "~&[JOIN FAIL 474 banned] ~A ~A"
                                   channel (message-params msg))
                         (setf (connection-state conn) :join-failed))
                    (475 (log:warn "~&[JOIN FAIL 475 bad-channel-key] ~A ~A"
                                   channel (message-params msg))
                         (setf (connection-state conn) :join-failed))
                    ;; RPL_NAMREPLY: populate user state
                    (353 (handle-namreply conn msg)))))

      ;; --- Protocol processing hooks ---
      (add-hook connection 'on-privmsg #'threaded-msg-hook)
      (add-hook connection 'on-ctcp #'threaded-action-hook)
      (add-hook connection 'on-quit #'threaded-byebye-hook)
      (add-hook connection 'on-part #'threaded-byebye-hook)
      (add-hook connection 'on-notice #'notice-tracker)
      ;; (add-hook connection 'on-nick #'irc-nick-change)

      ;; Connect to IRC. clatter-irc starts the read loop in its own thread.
      ;; We block this thread by joining the read thread so the calling code
      ;; can detect when the connection drops (matching old cl-irc behavior).
      (connect connection)
      (let ((read-thread (clatter-irc:connection-read-thread connection)))
        (when (and read-thread (bt:thread-alive-p read-thread))
          (bt:join-thread read-thread))))))

(defun make-bot-connection (nickname ircserver connection-port
                            &key nickserv-password nickserv-email port)
  "Create a bot-irc-connection (but do not connect yet).
NICKSERV-PASSWORD, when non-nil, triggers a NickServ IDENTIFY after RPL_WELCOME.
NICKSERV-EMAIL, when non-nil along with NICKSERV-PASSWORD, enables automatic
REGISTER if the nick is not yet registered.
PORT, when a valid TCP port number (1-65535), connects to that specific port
regardless of CONNECTION-PORT; useful for non-standard ports and test servers.
The actual TCP connection is established later by the thunk calling (connect)."
  (let ((connection (make-connection ircserver nickname
                                     :port (cond
                                             ((and (integerp port) (<= 1 port 65535)) port)
                                             ((eq connection-port :ssl) 6697)
                                             (t *default-port*))
                                     :tls (eq connection-port :ssl)
                                     :reconnect nil
                                     :connection-class 'bot-irc-connection)))
    (when nickserv-password
      (setf (nickserv-password connection) nickserv-password))
    (when nickserv-email
      (setf (nickserv-email connection) nickserv-email))
    (setf (gethash
           (list (string-upcase ircserver)
                 (string-upcase nickname))
           *irc-connections*) connection)
    (values connection)))

(defun kill-bot-connection (nickname ircserver)
  (let* ((nickname (string-upcase nickname))
	 (ircserver (string-upcase ircserver))
	 (k (list ircserver nickname))
	 (connection (gethash k *irc-connections*)))
    (when connection
      (disconnect connection "I'm tired. I'm going home.")
      (stop-task (mq-task connection))
      (sleep 1)
      ;; The bot-irc-client-thread blocks on join-thread of the read thread.
      ;; disconnect already stopped the read thread, so it should unblock.
      ;; If it's still alive, destroy it.
      (let ((thread (bot-irc-client-thread connection)))
        (when (and thread (bt:thread-alive-p thread))
          (bt:destroy-thread thread)))
      (remhash k *irc-connections*))))

(defun start-threaded-irc-client-instance (ircserver connection-port nickname
                                            channel channel-key
                                            &key nickserv-password nickserv-email)
  "Make a connection to an IRC server and spawn a thread to service it."
  (let ((connection (make-bot-connection nickname ircserver connection-port
                                         :nickserv-password nickserv-password
                                         :nickserv-email nickserv-email)))
    (make-thread
     (make-irc-client-instance-thunk nickname channel channel-key ircserver connection)
     :name (format nil "IRC Client thread: server ~A, nick ~A"
		   ircserver nickname))))

(defparameter *inter-connection-delay* 2
  "Seconds to wait between spawning IRC connection threads.
Staggering prevents a burst of simultaneous TLS handshakes from
triggering server-side flood protection.")

(defun start-threaded-irc-client-instances (&key (go? nil))
  "Spawn a thread to run a session with an IRC server.
Connections are staggered by *INTER-CONNECTION-DELAY* seconds."
  (dolist (cs (connections *bot-config*))
    (let ((connection-port (if (cs-ssl cs) :ssl :default)))
      (log:debug "~&IRC Server for join: ~A ~A nick=~A channel=~A"
                 (cs-server cs) connection-port (cs-nick cs) (cs-channel cs))
      (when go?
        (start-threaded-irc-client-instance
         (cs-server cs) connection-port (cs-nick cs)
         (cs-channel cs) (cs-channel-key cs)
         :nickserv-password (cs-nickserv-password cs)
         :nickserv-email (cs-nickserv-email cs))
        (sleep *inter-connection-delay*)))))

(defun print-bot-config (botconfig)
  "Print the connection specs in BOTCONFIG."
  (dolist (cs (connections botconfig))
    (log:debug "~&server=~A ssl=~A nick=~A channel=~A key=~A port=~A"
               (cs-server cs) (cs-ssl cs) (cs-nick cs)
               (cs-channel cs) (cs-channel-key cs) (cs-web-port cs))))

(defun stop-threaded-irc-client-instances ()
  "Shut down a session with an IRC server, and clean up."
  (stop-irc-client-instances))
