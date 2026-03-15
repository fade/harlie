;;;; test/db/users.lisp
;;;;
;;;; Integration tests for the harlie-user, bot-channel, and channel-user DAOs,
;;;; and the higher-level helpers that wrap them.

(uiop:define-package #:harlie/test/db/users
  (:use #:cl #:harlie/test/db/fixture)
  (:import-from #:harlie
                #:harlie-user
                #:harlie-user-id
                #:harlie-user-name
                #:current-handle
                #:authenticated?
                #:bot-channel
                #:bot-channel-id
                #:channel-name
                #:channel-user
                #:channel-id
                #:user-id
                #:ignored
                #:make-bot-channel
                #:get-bot-channel-for-name
                #:get-user-for-handle
                #:get-channel-user-mapping)
  (:local-nicknames (#:tt #:parachute)
                    (#:pomo #:postmodern)))

(in-package #:harlie/test/db/users)

;;;; ---- cleanup helpers --------------------------------------------------

(defun delete-test-user (handle)
  (pomo:execute (:delete-from 'harlie-user
                 :where (:= 'current-handle handle))))

(defun delete-test-channel (name)
  (pomo:execute (:delete-from 'bot-channel
                 :where (:= 'channel-name name))))

;;;; ---- test suite -------------------------------------------------------

(tt:define-test "harlie.db.users")

;;; ---- bot-channel -------------------------------------------------------

(tt:define-test "make-bot-channel/creates-row"
  :parent "harlie.db.users"
  :description "make-bot-channel returns a bot-channel DAO and persists it."
  (with-test-db
    (unwind-protect
         (let ((bc (make-bot-channel "#test-channel-1" "irc.test.invalid")))
           (tt:true (typep bc 'bot-channel))
           (tt:is string= "#test-channel-1" (channel-name bc))
           (tt:true (integerp (bot-channel-id bc))))
      (delete-test-channel "#test-channel-1"))))

(tt:define-test "make-bot-channel/idempotent"
  :parent "harlie.db.users"
  :description "Calling make-bot-channel twice for the same name returns the same row."
  (with-test-db
    (unwind-protect
         (let* ((bc1 (make-bot-channel "#test-channel-2" "irc.test.invalid"))
                (bc2 (make-bot-channel "#test-channel-2" "irc.test.invalid")))
           (tt:is = (bot-channel-id bc1) (bot-channel-id bc2)))
      (delete-test-channel "#test-channel-2"))))

;;; ---- harlie-user -------------------------------------------------------

(tt:define-test "get-user-for-handle/creates-user"
  :parent "harlie.db.users"
  :description "get-user-for-handle returns a harlie-user DAO, creating it if absent."
  (with-test-db
    (unwind-protect
         (let ((u (get-user-for-handle "test-handle-1")))
           (tt:true (typep u 'harlie-user))
           (tt:is string= "test-handle-1" (current-handle u))
           (tt:true (integerp (harlie-user-id u))))
      (delete-test-user "test-handle-1"))))

(tt:define-test "get-user-for-handle/idempotent"
  :parent "harlie.db.users"
  :description "Calling get-user-for-handle twice returns the same row."
  (with-test-db
    (unwind-protect
         (let* ((u1 (get-user-for-handle "test-handle-2"))
                (u2 (get-user-for-handle "test-handle-2")))
           (tt:is = (harlie-user-id u1) (harlie-user-id u2)))
      (delete-test-user "test-handle-2"))))

;;; ---- channel-user ------------------------------------------------------

(tt:define-test "get-channel-user-mapping/creates-mapping"
  :parent "harlie.db.users"
  :description "get-channel-user-mapping returns a channel-user DAO linking the two."
  (with-test-db
    (unwind-protect
         (let* ((bc  (make-bot-channel "#test-channel-3" "irc.test.invalid"))
                (u   (get-user-for-handle "test-handle-3"))
                (c/u (get-channel-user-mapping bc u)))
           (tt:true (typep c/u 'channel-user))
           (tt:is = (bot-channel-id bc) (channel-id c/u))
           (tt:is = (harlie-user-id u)  (user-id c/u))
           (tt:false (ignored c/u)))
      (delete-test-user "test-handle-3")
      (delete-test-channel "#test-channel-3"))))

(tt:define-test "get-channel-user-mapping/idempotent"
  :parent "harlie.db.users"
  :description "Calling get-channel-user-mapping twice returns the same row."
  (with-test-db
    (unwind-protect
         (let* ((bc   (make-bot-channel "#test-channel-4" "irc.test.invalid"))
                (u    (get-user-for-handle "test-handle-4"))
                (c/u1 (get-channel-user-mapping bc u))
                (c/u2 (get-channel-user-mapping bc u)))
           (tt:is = (channel-id c/u1) (channel-id c/u2))
           (tt:is = (user-id c/u1)    (user-id c/u2)))
      (delete-test-user "test-handle-4")
      (delete-test-channel "#test-channel-4"))))
