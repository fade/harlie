;;;; plugins.lisp

(in-package #:harlie)

(defvar *plugins* nil)

(defmacro defplugin (funame args &rest body)
  `(push (cons (symbol-name (quote ,funame)) (lambda (,@args) ,@body)) *plugins*))

(defplugin sources (reply-to token-list)
  (declare (ignore reply-to token-list))
  (format nil "git@coruscant.deepsky.com:harlie.git"))

(defplugin status (reply-to token-list)
  (declare (ignore reply-to token-list))
  (format nil "I know no phrases."))

(defplugin conv (reply-to token-list)
  (declare (ignore reply-to))
  (let* ((amount (second token-list))
	 (from (third token-list))
	 (to (fourth token-list))
	 (forex (find-forex (fetch-formatted-url
			     "http://www.xe.com/ucc/convert/?Amount=~A&From=~A&To=~A"
			     amount from to))))
    (format nil "~A = ~A" (first forex) (second forex))))

(defplugin jcw (reply-to token-list)
  (declare (ignore reply-to token-list))
  (format nil "FUCK YOU, JACKHOLE!"))

(defplugin rally (reply-to token-list)
  (declare (ignore reply-to token-list))
  (format nil "FUCK YOU, HANS!"))

(defplugin f1 (reply-to token-list)
  (declare (ignore reply-to token-list))
  (format nil "FUCK YOU, SCHUMACHER!"))

(defplugin spew (reply-to token-list)
  (declare (ignore reply-to token-list))
  (list "I'm a mouthy bastard" "Who can't get everything" "He wants to say" "In one line."))

(defplugin ccode (reply-to token-list)
  (declare (ignorable reply-to))
  (let* ((countries (country-lookup (second token-list))))
    (if (and countries (listp countries))
	(loop for (a . b) in countries
	      :collect (format nil "[ ~a ][ ~a ]" a b))
	(format nil "No match for search term: ~A" (second token-list)))))

(defplugin iata (reply-to token-list)
  (declare (ignorable reply-to))
  (let ((airports (airport-lookup (second token-list))))
    (if (and airports (listp airports))
	(loop for (a . b) in airports
	      :collect (format nil "[ ~A ][ ~A ]" a b))
	(format nil "No match for your airport: ~A" (second token-list)))))

(defun run-plugin (botcmd connection reply-to token-list)
  (let* ((plugname (string-upcase (subseq botcmd 1)))
	 (plugf (assoc plugname *plugins* :test #'string=)))
    (if plugf
	(let ((reply (funcall (cdr plugf) reply-to token-list)))
	  (cond ((stringp reply)
		 (privmsg connection reply-to
			  (format nil "~A:: ~A" (string-downcase plugname) reply)))
		((listp reply)
		 (dolist (line reply)
		   (privmsg connection reply-to
			    (format nil "~A:: ~A" (string-downcase plugname) line))))
		(t (privmsg connection reply-to
			    (format nil "~A:: I'm a tragic victim of duck typing gone wrong." (string-downcase plugname)))))))))
	(privmsg connection reply-to (format nil "~A: unknown command." (string-downcase plugname))))))
