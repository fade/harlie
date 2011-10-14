;;;; harlie.lisp

(in-package #:harlie)

(defvar *connection* (connect :nickname "Harlie" :server "irc.srh.org"))

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
  (let* ((channel (car (arguments message)))
	 (text (second (arguments message)))
	 (botcmd (car (space-split-string text))))
    (format t "~A ~A ~A~%" channel text botcmd)
    (cond ((equal botcmd "!sources")
	   (privmsg (connection message) channel "git@coruscant.deepsky.com:harlie.git"))
	  (t (privmsg (connection message) channel "Finest kind.")))))

(defun run-bot-instance ()
  (cl-irc:join *connection* "#trinity")
  (add-hook *connection* 'irc::irc-privmsg-message 'msg-hook)
  (read-message-loop *connection*))

(defun run-bot ()
  (make-thread #'run-bot-instance)
  )
