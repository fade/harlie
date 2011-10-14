;;;; harlie.lisp

(in-package #:harlie)

(defvar *connection* (connect :nickname "Harlie" :server "irc.srh.org"))

(defvar *last-message* nil)

(defun msg-hook (message)
  (setf *last-message* message)
  (let ((channel (car (arguments message)))
	(text (car (cdr (arguments message)))))
    (format t "~A ~A~%" channel text)
    (privmsg (connection message) (car (arguments message)) "Finest kind.")
    ))

(defun run-bot-instance ()
  (cl-irc:join *connection* "#trinity")
  (add-hook *connection* 'irc::irc-privmsg-message 'msg-hook)
  (read-message-loop *connection*))

(defun run-bot ()
  (make-thread #'run-bot-instance)
  )
