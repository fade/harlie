;;;; harlie.lisp

(in-package #:harlie)

(defun run-bot ()
  "Start up the IRC client and the Web servers."
  (setf *random-state* (make-random-state t))
  (start-web-client)
  (start-web-servers)
  (start-threaded-irc-client-instances))

(defun kill-bot ()
  "Disconnect from the IRC server, clean up persistent data, shut down the Web servers."
  (stop-threaded-irc-client-instances)
  (stop-web-servers)
  (stop-web-client))
