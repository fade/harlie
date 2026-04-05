;;;; run-consort.lisp — Entrypoint for running Consort as a service.
;;;; Usage: sbcl --load ~/quicklisp/setup.lisp --load run-consort.lisp

(in-package #:cl-user)

;; Ensure ASDF can find harlie
(pushnew (truename "/home/glenn/SourceCode/harlie/")
         asdf:*central-registry* :test #'equal)

;; Also ensure clatter-irc is findable (local dependency)
(pushnew (truename "/home/glenn/SourceCode/clatter-irc/")
         asdf:*central-registry* :test #'equal)

(ql:quickload :harlie)

(format t "~&;; Consort starting up...~%")
(harlie:run-bot)

;; Keep the process alive (the bot runs in threads)
(format t "~&;; Consort is running. Press Ctrl-C to stop.~%")
(handler-case
    (loop (sleep 3600))
  (sb-sys:interactive-interrupt ()
    (format t "~&;; Shutting down Consort...~%")
    (harlie:kill-bot)
    (format t "~&;; Consort stopped.~%")
    (sb-ext:exit :code 0)))
