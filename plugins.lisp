;;;; plugins.lisp

(in-package #:harlie)

(defvar *plugins* nil)

(defmacro defplugin (funame args &rest body)
  `(push (cons (symbol-name (quote ,funame)) (lambda (,@args) ,@body)) *plugins*))

(defplugin sources (connection reply-to token-list)
  (declare (ignore token-list))
  (privmsg connection reply-to "git@coruscant.deepsky.com:harlie.git"))

(defplugin status (connection reply-to token-list)
  (declare (ignore token-list))
  (privmsg connection reply-to "I know no phrases."))

(defplugin conv (connection reply-to token-list)
  (let* ((amount (second token-list))
	 (from (third token-list))
	 (to (fourth token-list))
	 (forex (find-forex (fetch-formatted-url
			     "http://www.xe.com/ucc/convert/?Amount=~A&From=~A&To=~A"
			     amount from to))))
    (privmsg connection reply-to (format nil "~A = ~A" (first forex) (second forex)))))

(defplugin jcw (connection reply-to token-list)
  (declare (ignore token-list))
  (privmsg connection reply-to (format nil "FUCK YOU, JACKHOLE!")))

(defplugin rally (connection reply-to token-list)
  (declare (ignore token-list))
  (privmsg connection reply-to (format nil "FUCK YOU, HANS!")))

(defplugin f1 (connection reply-to token-list)
  (declare (ignore token-list))
  (privmsg connection reply-to (format nil "FUCK YOU, SCHUMACHER!")))

(defun run-plugin (botcmd connection reply-to token-list)
  (let* ((plugname (string-upcase (subseq botcmd 1)))
	 (plugf (assoc plugname *plugins* :test #'string=)))
    (format t "~A~%" plugname)
    (format t "~A~%" plugf)
    (format t "~A~%" *plugins*)
    (if plugf
	(funcall (cdr plugf) connection reply-to token-list)
	(privmsg connection reply-to (format nil "~A: unknown command." plugname)))))
