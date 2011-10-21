;;;; harlie.lisp

(in-package #:harlie)

(defun run-bot ()
  "Start up the IRC client and the Web servers."
  (setf *random-state* (make-random-state t))
  (start-threaded-irc-client-instance)
  (start-web-servers))

(defun kill-bot ()
  "Disconnect from the IRC server, clean up persistent data, shut down the Web servers."
  (stop-threaded-irc-client-instance)
  (stop-web-servers))
