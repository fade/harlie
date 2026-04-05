;;;; test/fake-irc-server.lisp
;;;;
;;;; A minimal TCP IRC server for use in integration tests.  It speaks
;;;; just enough of the IRC protocol to drive the bot's connection state
;;;; machine: it completes the NICK/USER handshake, sends RPL_WELCOME,
;;;; and delivers configurable NickServ NOTICE messages in response to
;;;; IDENTIFY and REGISTER commands.

(defpackage #:harlie/test/fake-irc-server
  (:use #:cl)
  (:export #:fake-irc-server
           #:start-fake-irc-server
           #:stop-fake-irc-server
           #:fis-port
           #:fis-received
           #:fis-received-p
           #:fis-wait-for
           #:fis-inject
           #:with-fake-irc-server))

(in-package #:harlie/test/fake-irc-server)

;;;; ---- Data structure -------------------------------------------------------

(defclass fake-irc-server ()
  ((port
    :reader fis-port
    :documentation "The localhost port the server is listening on.")
   (received
    :initform nil
    :accessor fis-received
    :documentation "All raw lines received from the client, newest first.")
   (lock
    :initform (bt:make-lock "fis-lock")
    :reader fis-lock)
   (listen-socket
    :initform nil
    :accessor fis-listen-socket)
   (server-thread
    :initform nil
    :accessor fis-server-thread)
   ;; Configurable NickServ responses.  Each is a complete raw IRC line
   ;; (without trailing newline) to send when the corresponding PRIVMSG
   ;; is received.  nil means send no response.
   (nickserv-identify-response
    :initarg :nickserv-identify-response
    :initform nil
    :accessor fis-nickserv-identify-response
    :documentation "Line sent when PRIVMSG NickServ :IDENTIFY is received.")
   (nickserv-not-registered-response
    :initarg :nickserv-not-registered-response
    :initform nil
    :accessor fis-nickserv-not-registered-response
    :documentation "Line sent instead of identify-response when nick is unknown.")
   (nickserv-register-response
    :initarg :nickserv-register-response
    :initform nil
    :accessor fis-nickserv-register-response
    :documentation "Line sent when PRIVMSG NickServ :REGISTER is received.")
   (client-stream
    :initform nil
    :accessor fis-client-stream
    :documentation "The connected client's stream, set once accepted.")))

;;;; ---- Server loop ----------------------------------------------------------

(defun strip-cr (line)
  "Remove a trailing carriage return if present (IRC uses CRLF)."
  (if (and (> (length line) 0)
           (char= (char line (1- (length line))) #\Return))
      (subseq line 0 (1- (length line)))
      line))

(defun send-line (stream line)
  (write-string line stream)
  (write-char #\Return stream)
  (write-char #\Newline stream)
  (finish-output stream))

(defun run-server-loop (server client-socket)
  (let ((stream (usocket:socket-stream client-socket))
        (nick nil))
    (setf (fis-client-stream server) stream)
    (loop
      (let ((raw (read-line stream nil nil)))
        (when (null raw) (return))
        (let ((line (strip-cr raw)))
          (bt:with-lock-held ((fis-lock server))
            (push line (fis-received server)))
          (cond
            ;; CAP LS: clatter-irc sends this during registration.
            ;; Reply with empty capabilities so it proceeds with CAP END.
            ((cl-ppcre:scan "^CAP LS" line)
             (send-line stream
                        (format nil ":fake.irc.server CAP * LS :")))

            ;; CAP REQ / CAP END: silently absorb.
            ((cl-ppcre:scan "^CAP " line)
             nil)

            ((cl-ppcre:scan "^NICK " line)
             (let ((raw (string-trim '(#\Space) (subseq line 5))))
               (setf nick (if (and (> (length raw) 0) (char= (char raw 0) #\:))
                              (subseq raw 1)
                              raw))))

            ((cl-ppcre:scan "^USER " line)
             (let ((n (or nick "unknown")))
               (send-line stream
                          (format nil ":fake.irc.server 001 ~A :Welcome to the fake IRC network ~A" n n))))

            ((cl-ppcre:scan "(?i)^PRIVMSG NickServ :IDENTIFY" line)
             (let ((resp (or (fis-nickserv-identify-response server)
                             (fis-nickserv-not-registered-response server))))
               (when resp
                 (send-line stream resp))))

            ((cl-ppcre:scan "(?i)^PRIVMSG NickServ :REGISTER" line)
             (when (fis-nickserv-register-response server)
               (send-line stream (fis-nickserv-register-response server))))

            ;; JOIN: confirm the join so the bot transitions to :joined.
            ;; clatter-irc sends "JOIN #channel" (no trailing colon);
            ;; strip the leading colon if present for backward compat.
            ((cl-ppcre:scan "^JOIN " line)
             (let* ((raw-channel (string-trim '(#\Space) (subseq line 5)))
                    (channel (if (and (> (length raw-channel) 0)
                                      (char= (char raw-channel 0) #\:))
                                 (subseq raw-channel 1)
                                 raw-channel)))
               (send-line stream
                          (format nil ":~A!~A@127.0.0.1 JOIN ~A"
                                  (or nick "unknown")
                                  (or nick "unknown")
                                  channel))))

            ;; Absorb QUIT, PING, etc. silently.
            (t nil)))))))

;;;; ---- Public API -----------------------------------------------------------

(defun start-fake-irc-server (server)
  "Bind to a random localhost port and start accepting one connection."
  (let ((sock (usocket:socket-listen "127.0.0.1" 0 :reuse-address t)))
    (setf (fis-listen-socket server) sock)
    (setf (slot-value server 'port) (usocket:get-local-port sock))
    (setf (fis-server-thread server)
          (bt:make-thread
           (lambda ()
             (unwind-protect
                  (let ((client (usocket:socket-accept sock)))
                    (unwind-protect
                         (run-server-loop server client)
                      (usocket:socket-close client)))
               (ignore-errors (usocket:socket-close sock))))
           :name "fake-irc-server")))
  server)

(defun stop-fake-irc-server (server)
  "Shut down the fake server and its thread."
  (when (fis-server-thread server)
    (ignore-errors (bt:destroy-thread (fis-server-thread server)))
    (setf (fis-server-thread server) nil))
  (when (fis-listen-socket server)
    (ignore-errors (usocket:socket-close (fis-listen-socket server)))
    (setf (fis-listen-socket server) nil)))

(defun fis-inject (server line)
  "Send LINE from the server to the connected client.
   Used in tests to simulate other users joining, speaking, etc."
  (let ((stream (fis-client-stream server)))
    (when stream
      (send-line stream line)
      t)))

(defun fis-received-p (server substring)
  "Return the first received line containing SUBSTRING, or NIL."
  (bt:with-lock-held ((fis-lock server))
    (find substring (fis-received server) :test #'search)))

(defun fis-wait-for (server substring &optional (timeout 8))
  "Block until SERVER has received a line containing SUBSTRING, or TIMEOUT seconds pass.
Returns T if the line was received, NIL on timeout."
  (let ((deadline (+ (get-universal-time) timeout)))
    (loop
      (when (fis-received-p server substring) (return t))
      (when (> (get-universal-time) deadline) (return nil))
      (sleep 0.05))))

(defmacro with-fake-irc-server ((var &rest initargs) &body body)
  "Bind VAR to a started fake-irc-server, ensuring cleanup on exit."
  `(let ((,var (start-fake-irc-server
                (make-instance 'fake-irc-server ,@initargs))))
     (unwind-protect
          (progn ,@body)
       (stop-fake-irc-server ,var))))
