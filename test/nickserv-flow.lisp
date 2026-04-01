;;;; test/nickserv-flow.lisp
;;;;
;;;; Integration tests for the NickServ authentication and registration
;;;; state machine in make-irc-client-instance-thunk.
;;;;
;;;; Each test:
;;;;   1. Starts a fake IRC server with a configurable NickServ personality.
;;;;   2. Connects a bot-irc-connection against it on localhost.
;;;;   3. Waits for the expected connection-state, with a timeout.
;;;;   4. Asserts on the state and on the commands the fake server received.

(uiop:define-package #:harlie/test/nickserv-flow
  (:use #:cl #:harlie/test/fake-irc-server)
  (:import-from #:harlie
                #:make-bot-connection
                #:make-irc-client-instance-thunk
                #:bot-state
                #:*irc-connections*)
  (:import-from #:clatter-irc #:disconnect)
  (:import-from #:bordeaux-threads #:make-thread #:destroy-thread)
  (:local-nicknames (#:tt #:parachute)))

(in-package #:harlie/test/nickserv-flow)

;;;; ---- Helpers --------------------------------------------------------------

(defparameter *state-poll-interval* 0.05
  "Seconds between state polls in WAIT-FOR-STATE.")

(defparameter *state-timeout* 8
  "Seconds before WAIT-FOR-STATE gives up.")

(defun wait-for-state (connection target-state &optional (timeout *state-timeout*))
  "Poll CONNECTION's state until it equals TARGET-STATE or TIMEOUT seconds pass.
Returns T if the state was reached, NIL on timeout."
  (let ((deadline (+ (get-universal-time) timeout)))
    (loop
      (when (eq (bot-state connection) target-state)
        (return t))
      (when (> (get-universal-time) deadline)
        (return nil))
      (sleep *state-poll-interval*))))

(defun make-test-connection (server nick &key nickserv-password nickserv-email)
  "Create a bot-irc-connection against the fake SERVER and start its
message-loop thread.  Returns (values connection thread)."
  (let* ((connection (make-bot-connection nick "127.0.0.1" :default
                                          :port (fis-port server)
                                          :nickserv-password nickserv-password
                                          :nickserv-email nickserv-email))
         (thread (bt:make-thread
                  (make-irc-client-instance-thunk
                   nick "#test-channel" nil "127.0.0.1" connection)
                  :name (format nil "test-irc-thread/~A" nick))))
    (values connection thread)))

(defun cleanup-test-connection (nick)
  "Remove the test connection from *irc-connections*."
  (remhash (list "127.0.0.1" (string-upcase nick)) *irc-connections*))

(defmacro with-test-connection ((conn-var nick &rest connect-args) server &body body)
  "Run BODY with CONN-VAR bound to a test connection against SERVER.
Cleans up the connection and its thread on exit."
  (let ((thread-var (gensym "THREAD")))
    `(multiple-value-bind (,conn-var ,thread-var)
         (make-test-connection ,server ,nick ,@connect-args)
       (declare (ignorable ,thread-var))
       (unwind-protect
            (progn ,@body)
         (ignore-errors (disconnect ,conn-var "test cleanup"))
         (sleep 0.1)
         (ignore-errors (bt:destroy-thread ,thread-var))
         (cleanup-test-connection ,nick)))))

;;;; ---- NickServ NOTICE helper strings --------------------------------------

(defun nickserv-notice (nick text)
  (format nil ":NickServ!NickServ@services.fake.irc NOTICE ~A :~A" nick text))

;;;; ---- Test suite ----------------------------------------------------------

(tt:define-test "harlie.nickserv")

(tt:define-test "no-password-joins-directly"
  :parent "harlie.nickserv"
  :description "When no nickserv-password is set the bot joins immediately after RPL_WELCOME."
  (with-fake-irc-server (server)
    (with-test-connection (conn "Plonk") server
      (tt:true (fis-wait-for server "JOIN #test-channel"))
      ;; Must NOT have sent IDENTIFY.
      (tt:false (fis-received-p server "IDENTIFY")))))

(tt:define-test "identify-success-then-join"
  :parent "harlie.nickserv"
  :description "Bot identifies successfully and then joins the channel."
  (with-fake-irc-server (server
    :nickserv-identify-response
    (nickserv-notice "Plonk" "You are now logged in as Plonk"))
    (with-test-connection (conn "Plonk" :nickserv-password "s3kr3t") server
      (tt:true (fis-wait-for server "JOIN #test-channel"))
      (tt:true (fis-received-p server "IDENTIFY s3kr3t")))))

(tt:define-test "not-registered-without-email-joins-without-identification"
  :parent "harlie.nickserv"
  :description "When the nick is unregistered and no email is configured, the bot warns and joins anyway."
  (with-fake-irc-server (server
    :nickserv-not-registered-response
    (nickserv-notice "Plonk" "This nickname is not registered."))
    (with-test-connection (conn "Plonk" :nickserv-password "s3kr3t") server
      (tt:true (fis-wait-for server "JOIN #test-channel"))
      ;; Must NOT have attempted REGISTER.
      (tt:false (fis-received-p server "REGISTER")))))

(tt:define-test "not-registered-with-email-registers-then-joins"
  :parent "harlie.nickserv"
  :description "When the nick is unregistered and an email is configured, the bot registers and then joins."
  (with-fake-irc-server (server
    :nickserv-not-registered-response
    (nickserv-notice "Plonk" "This nickname is not registered.")
    :nickserv-register-response
    (nickserv-notice "Plonk" "An email containing account activation instructions has been sent to test@example.com."))
    (with-test-connection (conn "Plonk"
                                :nickserv-password "s3kr3t"
                                :nickserv-email "test@example.com")
        server
      (tt:true (fis-wait-for server "JOIN #test-channel"))
      (tt:true (fis-received-p server "REGISTER s3kr3t test@example.com")))))

(tt:define-test "identify-before-join"
  :parent "harlie.nickserv"
  :description "IDENTIFY must be sent before JOIN, not after."
  (with-fake-irc-server (server
    :nickserv-identify-response
    (nickserv-notice "Plonk" "You are now logged in as Plonk"))
    (with-test-connection (conn "Plonk" :nickserv-password "s3kr3t") server
      (tt:true (fis-wait-for server "JOIN #test-channel"))
      (let* ((lines (reverse (fis-received server)))
             (identify-pos (position-if (lambda (l) (search "IDENTIFY" l)) lines))
             (join-pos     (position-if (lambda (l) (search "JOIN" l)) lines)))
        (tt:true (and identify-pos join-pos (< identify-pos join-pos)))))))
