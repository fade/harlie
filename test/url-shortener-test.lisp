;;;; test/url-shortener-test.lisp
;;;;
;;;; Integration test for the binary URL shortening codepath.
;;;; Uses the fake-irc-server to exercise the full msg-hook → store-binary-url
;;;; → channel reply flow, with a mock URL store to avoid hitting the database.

(uiop:define-package #:harlie/test/url-shortener
  (:use #:cl #:harlie/test/fake-irc-server)
  (:import-from #:harlie
                #:make-bot-connection #:make-irc-client-instance-thunk
                #:bot-state #:*irc-connections* #:*the-url-store*
                #:get-filename-from-url
                #:store-binary-url #:lookup-url
                #:make-short-url-string)
  (:import-from #:clatter-irc #:disconnect)
  (:import-from #:bordeaux-threads #:make-thread #:destroy-thread)
  (:local-nicknames (#:tt #:parachute)))

(in-package #:harlie/test/url-shortener)

;;;; ---- Mock URL store -------------------------------------------------------
;;
;; Avoids all database access.  store-binary-url returns a canned short URL
;; and the "Binary File: <filename>" title, exactly as the real method does
;; after a successful insert.

(defclass fake-url-store () ()
  (:documentation "A mock URL store that returns predictable values without DB access."))

(defmethod store-binary-url ((store fake-url-store) context url nick)
  "Return a fake short URL and the binary-file title with filename."
  (values (make-short-url-string context "FAKE")
          (format nil "Binary File: ~A" (get-filename-from-url url))))

(defmethod lookup-url ((store fake-url-store) context url nick)
  "Return a fake short URL and a canned title for non-binary URLs."
  (values (make-short-url-string context "FAKE")
          "Fake Page Title"))

;;;; ---- Helpers --------------------------------------------------------------

(defun wait-for-bot-state (conn target-state &key (timeout 5))
  "Wait up to TIMEOUT seconds for CONN to reach TARGET-STATE."
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout internal-time-units-per-second))))
    (loop while (and (not (eq (bot-state conn) target-state))
                     (< (get-internal-real-time) deadline))
          do (sleep 0.05))
    (eq (bot-state conn) target-state)))

(defmacro with-fake-url-store (&body body)
  "Temporarily replace *the-url-store* with a fake-url-store, restoring on exit."
  (let ((saved (gensym "SAVED-STORE")))
    `(let ((,saved harlie::*the-url-store*))
       (unwind-protect
            (progn
              (setf harlie::*the-url-store* (make-instance 'fake-url-store))
              ,@body)
         (setf harlie::*the-url-store* ,saved)))))

;;;; ---- Test suite -----------------------------------------------------------

(tt:define-test "url-shortener-tests")

(tt:define-test "binary-url-returns-filename-in-title"
  :parent "url-shortener-tests"
  :description "When a user pastes a binary URL, the bot replies with 'Binary File: <filename>'."
  (with-fake-url-store
    (with-fake-irc-server (server)
      (let ((conn (make-bot-connection "TestBot" "127.0.0.1" :default
                                      :port (fis-port server))))
        (unwind-protect
             (progn
               (let ((bot-thread
                       (bt:make-thread
                        (make-irc-client-instance-thunk
                         "TestBot" "#test" nil "127.0.0.1" conn)
                        :name "url-test-bot")))
                 (declare (ignorable bot-thread))
                 ;; Wait for bot to join
                 (tt:true (fis-wait-for server "JOIN #test"))
                 (tt:true (wait-for-bot-state conn :joined))

                 ;; Simulate a user posting a binary URL (.png is in *binary-url-suffixes*)
                 (fis-inject server
                             ":someone!user@example.com PRIVMSG #test :check this out https://localhost/files/screenshot.png")

                 ;; The bot should reply with a PRIVMSG containing the filename
                 (tt:true (fis-wait-for server "Binary File: screenshot.png"))))
          ;; Cleanup
          (handler-case (disconnect conn "test done") (error () nil))
          (sleep 0.2)
          (remhash (list "127.0.0.1" "TESTBOT") *irc-connections*))))))

(tt:define-test "binary-url-different-suffixes"
  :parent "url-shortener-tests"
  :description "Various binary suffixes all produce the filename in the reply."
  (with-fake-url-store
    (with-fake-irc-server (server)
      (let ((conn (make-bot-connection "TestBot" "127.0.0.1" :default
                                      :port (fis-port server))))
        (unwind-protect
             (progn
               (let ((bot-thread
                       (bt:make-thread
                        (make-irc-client-instance-thunk
                         "TestBot" "#test" nil "127.0.0.1" conn)
                        :name "url-test-bot2")))
                 (declare (ignorable bot-thread))
                 (tt:true (fis-wait-for server "JOIN #test"))
                 (tt:true (wait-for-bot-state conn :joined))

                 ;; Post a .jpg URL
                 (fis-inject server
                             ":someone!user@example.com PRIVMSG #test :look https://cdn.localhost/photos/sunset.jpg")
                 (tt:true (fis-wait-for server "Binary File: sunset.jpg"))

                 ;; Post a .mp4 URL
                 (fis-inject server
                             ":someone!user@example.com PRIVMSG #test :video https://media.localhost/clips/cat.mp4")
                 (tt:true (fis-wait-for server "Binary File: cat.mp4"))))
          ;; Cleanup
          (handler-case (disconnect conn "test done") (error () nil))
          (sleep 0.2)
          (remhash (list "127.0.0.1" "TESTBOT") *irc-connections*))))))
