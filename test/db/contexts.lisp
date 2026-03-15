;;;; test/db/contexts.lisp
;;;;
;;;; Integration tests for make-context-entry and load-contexts.
;;;; Runs against harlie-test (never botdb).

(uiop:define-package #:harlie/test/db/contexts
  (:use #:cl #:harlie/test/db/fixture)
  (:import-from #:harlie
                #:make-context-entry
                #:load-contexts
                #:make-config
                #:make-connection-spec
                #:*bot-config*)
  (:local-nicknames (#:tt #:parachute)
                    (#:pomo #:postmodern)))

(in-package #:harlie/test/db/contexts)

;;;; ---- helpers ----------------------------------------------------------

(defun test-row-count (nick server channel)
  "Return the number of contexts rows matching the given triple."
  (pomo:query (:select (:count :*)
               :from 'contexts
               :where (:and (:= 'context-name nick)
                            (:= 'irc-server server)
                            (:= 'irc-channel channel)))
              :single))

(defun delete-test-rows (nick)
  "Remove all contexts rows whose context_name starts with NICK."
  (pomo:execute (:delete-from 'contexts
                 :where (:like 'context-name nick))))

;;;; ---- test suite -------------------------------------------------------

(tt:define-test "harlie.db.contexts")

(tt:define-test "make-context-entry/inserts-row"
  :parent "harlie.db.contexts"
  :description "A fresh insert creates exactly one row."
  (with-test-db
    (unwind-protect
         (progn
           (make-context-entry "test-bot" "irc.test.invalid" "#test-chan"
                               "web.test.invalid" 9000 "/")
           (tt:is = 1 (test-row-count "test-bot" "irc.test.invalid" "#test-chan")))
      (delete-test-rows "test-bot"))))

(tt:define-test "make-context-entry/idempotent"
  :parent "harlie.db.contexts"
  :description "Calling make-context-entry twice for the same key leaves exactly one row."
  (with-test-db
    (unwind-protect
         (progn
           (make-context-entry "test-bot" "irc.test.invalid" "#test-chan"
                               "web.test.invalid" 9000 "/")
           (make-context-entry "test-bot" "irc.test.invalid" "#test-chan"
                               "web.test.invalid" 9000 "/")
           (tt:is = 1 (test-row-count "test-bot" "irc.test.invalid" "#test-chan")))
      (delete-test-rows "test-bot"))))

(tt:define-test "make-context-entry/updates-on-conflict"
  :parent "harlie.db.contexts"
  :description "Re-inserting with a different web_port updates the existing row."
  (with-test-db
    (unwind-protect
         (progn
           (make-context-entry "test-bot" "irc.test.invalid" "#test-chan"
                               "web.test.invalid" 9000 "/")
           (make-context-entry "test-bot" "irc.test.invalid" "#test-chan"
                               "web.test.invalid" 9001 "/new")
           (let ((row (pomo:query (:select 'web-port 'web-uri-prefix
                                   :from 'contexts
                                   :where (:and (:= 'context-name "test-bot")
                                                (:= 'irc-server "irc.test.invalid")
                                                (:= 'irc-channel "#test-chan")))
                                  :row)))
             (tt:is = 9001 (first row))
             (tt:is string= "/new" (second row))))
      (delete-test-rows "test-bot"))))

(tt:define-test "load-contexts/upserts-all-specs"
  :parent "harlie.db.contexts"
  :description "load-contexts inserts one row per connection-spec in *bot-config*."
  (with-test-db
    (unwind-protect
         (let ((*bot-config*
                 (make-config
                  :db-credentials *test-db*
                  :web-server-name "web.test.invalid"
                  :connections (list
                                (make-connection-spec
                                 :server  "irc.test.invalid"
                                 :nick    "test-bot-a"
                                 :channel "#chan-a"
                                 :web-port 9010)
                                (make-connection-spec
                                 :server  "irc.test.invalid"
                                 :nick    "test-bot-b"
                                 :channel "#chan-b"
                                 :web-port 9011)))))
           (load-contexts)
           (tt:is = 1 (test-row-count "test-bot-a" "irc.test.invalid" "#chan-a"))
           (tt:is = 1 (test-row-count "test-bot-b" "irc.test.invalid" "#chan-b")))
      (delete-test-rows "test-bot-a")
      (delete-test-rows "test-bot-b"))))

(tt:define-test "load-contexts/idempotent"
  :parent "harlie.db.contexts"
  :description "Calling load-contexts twice still leaves exactly one row per spec."
  (with-test-db
    (unwind-protect
         (let ((*bot-config*
                 (make-config
                  :db-credentials *test-db*
                  :web-server-name "web.test.invalid"
                  :connections (list
                                (make-connection-spec
                                 :server  "irc.test.invalid"
                                 :nick    "test-bot-c"
                                 :channel "#chan-c"
                                 :web-port 9020)))))
           (load-contexts)
           (load-contexts)
           (tt:is = 1 (test-row-count "test-bot-c" "irc.test.invalid" "#chan-c")))
      (delete-test-rows "test-bot-c"))))
