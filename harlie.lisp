;;;; harlie.lisp

(in-package #:harlie)

(defvar *base-pathname*
  #.(uiop:pathname-directory-pathname (or *compile-file-truename* *load-truename*))
  "calculate this at run-time, not fixed at compile time...")

(defun slynky (&key (port 4007))
  (slynk:create-server :port port :dont-close t))

(defvar *boot-time* nil "Universal time when the bot was started.")

(defun run-bot ()
  "Start up the IRC client and the Web servers."
  (setf *boot-time* (get-universal-time))
  (setf *random-state* (make-random-state t))
  (start-web-client)
  (start-web-servers)
  ;; if the database isn't configured, do it.
  (initialize-startup-maybe :go? t)
  (load-memos-from-db)
  (sleep 3)
  (start-threaded-irc-client-instances :go? t)
  (start-cron)
  (add-canonical-crons)) ;; don't start slynk here. it doesn't work.


(defun kill-bot ()
  "Disconnect from the IRC server, clean up persistent data, shut down the Web servers."
  (stop-threaded-irc-client-instances)
  (setf *users* nil)
  (setf *random-state* nil)  
  (stop-web-servers)
  (stop-web-client))

