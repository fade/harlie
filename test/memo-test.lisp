;;;; test/memo-test.lisp — Tests for the !tell memo system
;;
;; Unit tests for memo storage, retrieval, formatting, and delivery.
;; Integration test with fake-irc-server for end-to-end !tell flow.

(uiop:define-package #:harlie/test/memo
  (:use #:cl #:harlie/test/fake-irc-server)
  (:import-from #:harlie
                #:store-memo #:pending-memos-for #:pending-memo-count
                #:has-pending-memos-p #:clear-all-memos
                #:format-memo-age #:format-memo-delivery
                #:deliver-memos-for #:pending-memo-sender
                #:pending-memo-recipient #:pending-memo-message
                #:pending-memo-channel #:pending-memo-created-at
                #:make-bot-connection #:make-irc-client-instance-thunk
                #:bot-state #:*irc-connections*)
  (:import-from #:clatter-irc #:disconnect)
  (:import-from #:bordeaux-threads #:make-thread #:destroy-thread)
  (:local-nicknames (#:tt #:parachute)))

(in-package #:harlie/test/memo)

;;; ============================================================
;;; Unit tests — memo storage and retrieval
;;; ============================================================

(tt:define-test "memo-unit-tests")

(tt:define-test "store-and-retrieve"
  :parent "memo-unit-tests"
  (clear-all-memos)
  (let ((memo (store-memo "Alice" "Bob" "#lisp" "check the PR")))
    (tt:true memo)
    (tt:is string= "Alice" (pending-memo-sender memo))
    (tt:is string= "Bob" (pending-memo-recipient memo))
    (tt:is string= "#lisp" (pending-memo-channel memo))
    (tt:is string= "check the PR" (pending-memo-message memo))
    ;; Retrievable
    (tt:true (has-pending-memos-p "Bob"))
    (tt:is = 1 (pending-memo-count "Bob"))
    ;; Case insensitive
    (tt:true (has-pending-memos-p "bob"))
    (tt:true (has-pending-memos-p "BOB")))
  (clear-all-memos))

(tt:define-test "multiple-memos"
  :parent "memo-unit-tests"
  (clear-all-memos)
  (store-memo "Alice" "Bob" "#lisp" "first message")
  (store-memo "Charlie" "Bob" "#lisp" "second message")
  (store-memo "Alice" "Bob" "#emacs" "third message")
  (tt:is = 3 (pending-memo-count "Bob"))
  (let ((memos (pending-memos-for "Bob")))
    (tt:is = 3 (length memos))
    ;; Most recent first (push order)
    (tt:is string= "third message" (pending-memo-message (first memos)))
    (tt:is string= "first message" (pending-memo-message (third memos))))
  (clear-all-memos))

(tt:define-test "no-self-memos"
  :parent "memo-unit-tests"
  (clear-all-memos)
  (tt:false (store-memo "Alice" "Alice" "#lisp" "talking to myself"))
  (tt:false (has-pending-memos-p "Alice"))
  (clear-all-memos))

(tt:define-test "no-empty-memos"
  :parent "memo-unit-tests"
  (clear-all-memos)
  (tt:false (store-memo "Alice" "Bob" "#lisp" ""))
  (tt:false (store-memo "Alice" "Bob" "#lisp" "   "))
  (tt:false (store-memo "Alice" nil "#lisp" "hello"))
  (tt:false (has-pending-memos-p "Bob"))
  (clear-all-memos))

(tt:define-test "no-memos-for-unknown"
  :parent "memo-unit-tests"
  (clear-all-memos)
  (tt:false (has-pending-memos-p "nobody"))
  (tt:is = 0 (pending-memo-count "nobody"))
  (clear-all-memos))

;;; ============================================================
;;; Unit tests — age formatting
;;; ============================================================

(tt:define-test "format-age"
  :parent "memo-unit-tests"
  (tt:is string= "just now" (format-memo-age 30))
  (tt:is string= "1 minute" (format-memo-age 60))
  (tt:is string= "5 minutes" (format-memo-age 300))
  (tt:is string= "1 hour" (format-memo-age 3600))
  (tt:is string= "2 hours" (format-memo-age 7200))
  (tt:is string= "1 day" (format-memo-age 86400))
  (tt:is string= "3 days" (format-memo-age 259200)))

;;; ============================================================
;;; Unit tests — memo delivery formatting
;;; ============================================================

(tt:define-test "delivery-format"
  :parent "memo-unit-tests"
  (clear-all-memos)
  (let ((memo (store-memo "Alice" "Bob" "#lisp" "check the PR")))
    ;; Overwrite created-at to control the age
    (setf (pending-memo-created-at memo) (- (get-universal-time) 3600))
    (let ((text (format-memo-delivery memo)))
      (tt:true (search "Bob:" text))
      (tt:true (search "Alice" text))
      (tt:true (search "1 hour ago" text))
      (tt:true (search "check the PR" text))))
  (clear-all-memos))

;;; ============================================================
;;; Integration test — !tell through fake IRC server
;;; ============================================================

(tt:define-test "memo-integration-tests")

(defun wait-for-bot-state (conn target-state &key (timeout 5))
  "Wait up to TIMEOUT seconds for CONN to reach TARGET-STATE."
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout internal-time-units-per-second))))
    (loop while (and (not (eq (bot-state conn) target-state))
                     (< (get-internal-real-time) deadline))
          do (sleep 0.05))
    (eq (bot-state conn) target-state)))

(tt:define-test "tell-and-deliver-on-join"
  :parent "memo-integration-tests"
  "Full flow: User A sends !tell for User B, User B joins, memo is delivered."
  (clear-all-memos)
  (with-fake-irc-server (server)
    (let ((conn (make-bot-connection "TestBot" "127.0.0.1" :default
                                    :port (fis-port server))))
      (unwind-protect
           (progn
             (let ((bot-thread
                     (bt:make-thread
                      (make-irc-client-instance-thunk
                       "TestBot" "#test" nil "127.0.0.1" conn)
                      :name "memo-test-bot")))
               (declare (ignorable bot-thread))
               ;; Wait for bot to join
               (tt:true (fis-wait-for server "JOIN #test"))
               (tt:true (wait-for-bot-state conn :joined))

               ;; Store a memo (simulating what !tell plugin does)
               (store-memo "Alice" "Bob" "#test" "check the PR please")
               (tt:true (has-pending-memos-p "Bob"))

               ;; Simulate Bob joining the channel
               (fis-inject server ":Bob!bob@example.com JOIN #test")
               ;; Give delivery hooks time to fire
               (sleep 0.5)

               ;; Memo should have been delivered
               (tt:false (has-pending-memos-p "Bob"))

               ;; Verify the bot sent a PRIVMSG with the memo content
               (tt:true (fis-wait-for server "Alice left a message"))))
        ;; Cleanup
        (handler-case (disconnect conn "test done") (error () nil))
        (sleep 0.2)
        (clear-all-memos)
        (remhash (list "127.0.0.1" "TESTBOT") *irc-connections*)))))

(tt:define-test "tell-and-deliver-on-speak"
  :parent "memo-integration-tests"
  "User A stores memo for User B who is already in channel. B speaks, memo delivered."
  (clear-all-memos)
  (with-fake-irc-server (server)
    (let ((conn (make-bot-connection "TestBot" "127.0.0.1" :default
                                    :port (fis-port server))))
      (unwind-protect
           (progn
             (let ((bot-thread
                     (bt:make-thread
                      (make-irc-client-instance-thunk
                       "TestBot" "#test" nil "127.0.0.1" conn)
                      :name "memo-test-bot2")))
               (declare (ignorable bot-thread))
               (tt:true (fis-wait-for server "JOIN #test"))
               (tt:true (wait-for-bot-state conn :joined))

               ;; Store a memo for Charlie
               (store-memo "Dave" "Charlie" "#test" "meeting moved to 3pm")
               (tt:true (has-pending-memos-p "Charlie"))

               ;; Simulate Charlie speaking in the channel
               (fis-inject server ":Charlie!charlie@example.com PRIVMSG #test :hello everyone")
               (sleep 0.5)

               ;; Memo should be delivered
               (tt:false (has-pending-memos-p "Charlie"))
               (tt:true (fis-wait-for server "Dave left a message"))))
        ;; Cleanup
        (handler-case (disconnect conn "test done") (error () nil))
        (sleep 0.2)
        (clear-all-memos)
        (remhash (list "127.0.0.1" "TESTBOT") *irc-connections*)))))

(tt:define-test "no-delivery-when-no-memos"
  :parent "memo-integration-tests"
  "User joins with no pending memos — nothing extra is sent."
  (clear-all-memos)
  (with-fake-irc-server (server)
    (let ((conn (make-bot-connection "TestBot" "127.0.0.1" :default
                                    :port (fis-port server))))
      (unwind-protect
           (progn
             (let ((bot-thread
                     (bt:make-thread
                      (make-irc-client-instance-thunk
                       "TestBot" "#test" nil "127.0.0.1" conn)
                      :name "memo-test-bot3")))
               (declare (ignorable bot-thread))
               (tt:true (fis-wait-for server "JOIN #test"))
               (tt:true (wait-for-bot-state conn :joined))

               ;; Nobody has memos
               (tt:false (has-pending-memos-p "Eve"))

               ;; Simulate Eve joining
               (fis-inject server ":Eve!eve@example.com JOIN #test")
               (sleep 0.3)

               ;; No memo delivery message should have been sent
               (tt:false (fis-received-p server "left a message"))))
        ;; Cleanup
        (handler-case (disconnect conn "test done") (error () nil))
        (sleep 0.2)
        (clear-all-memos)
        (remhash (list "127.0.0.1" "TESTBOT") *irc-connections*)))))
