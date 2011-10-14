;;;; harlie.lisp

(in-package #:harlie)

(defvar *my-nick* "Harlie")

(defvar *connection* (connect :nickname *my-nick* :server "irc.srh.org"))

(defvar *last-message* nil)

(defun split-2 (str pos)
  (if pos
      (list (subseq str 0 pos) (subseq str (1+ pos)))
      str))

(defun space-split-string (str &optional acc)
  (let* ((splat (string-trim (list #\Space) str))
	(splotch (split-2 splat (position #\Space splat))))
    (if (listp splotch)
	(space-split-string (string-left-trim (list #\Space) (second splotch)) (append acc (list (first splotch))))
	(append acc (list splat)))))

(defun msg-hook (message)
  (setf *last-message* message)
  (let* ((channel (string-upcase (car (arguments message))))
	 (connection (connection message))
	 (text (second (arguments message)))
	 (botcmd (string-upcase (car (space-split-string text))))
	 (reply-to channel))
    (progn
      (format t "Is this shit getting through?~%")
      (format t "Message: |~A|~%" (raw-message-string message))
      (format t "   connection=~A channel=~A~%" connection channel)
      (if (equal channel (string-upcase *my-nick*))
	  (setf reply-to (user message)))
      (cond ((equal botcmd "!SOURCES")
	     (privmsg connection reply-to "git@coruscant.deepsky.com:harlie.git"))
	    (t (privmsg connection reply-to "Finest kind."))))))

(defun run-bot-instance ()
  (cl-irc:join *connection* "#trinity")
  (add-hook *connection* 'irc::irc-privmsg-message 'msg-hook)
  (read-message-loop *connection*))

(defun run-bot ()
  (make-thread #'run-bot-instance)
  )
