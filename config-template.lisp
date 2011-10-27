;;;; config-template.lisp

(in-package :harlie)

;; With respect to David Gerrold's _When HARLIE Was One_
(defparameter *my-irc-nick* "Harlie")

(defparameter *irc-server-name* "irc.srh.org")

(defparameter *irc-channel-names* '("#trinity"))

(defparameter *web-server-port* 5791)

(defparameter *web-server-name* "127.0.0.1")

(defun make-url-prefix ()
  (if (eql 80 *web-server-port*)
      (format nil "http://~A/"
	      *web-server-name*)
      (format nil "http://~A:~A/"
	      *web-server-name*
	      *web-server-port*)))

(defparameter *url-prefix* (make-url-prefix))

(defparameter *url-store-type* :hash)

(defparameter *psql-old-credentials*
  '(("botdb" "semaphor" nil :unix)
    ("bootsydb" "semaphor" nil :unix)
    ("shogundb" "semaphor" nil :unix)
    ("thugdb" "semaphor" nil :unix)))

(defparameter *psql-new-credentials* '("harliedb" "semaphor" nil :unix))

(defparameter *chain-db* '("botdb" "semaphor" nil :unix))

(defun (set-shogun)
    (progn
      (setf *my-irc-nick* "shogun")
      (setf *irc-channel-names* '("#walled"))
      (setf *web-server-port* 5783)
      (setf *web-server-name* "coruscant.deepsky.com")
      (setf *url-prefix* (make-url-prefix))
      (setf *url-store-type* :psql)
      (setf *psql-new-credentials* '("shogundb" "semaphor" nil :unix))
      (setf *chain-db* '("shogundb" "semaphor" nil :unix))))
