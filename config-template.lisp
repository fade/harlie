;;;; config-template.lisp

(in-package :harlie)

;; With respect to David Gerrold's _When HARLIE Was One_
(defparameter *my-irc-nick* "Harlie")

(defparameter *irc-server-name* "irc.srh.org")

(defparameter *web-server-port* 5791)

(defparameter *web-server-name* "127.0.0.1")

(defparameter *url-prefix*
  (if (eql 80 *web-server-port*)
      (format nil "http://~A/"
	      *web-server-name*)
      (format nil "http://~A:~A/"
	      *web-server-name*
	      *web-server-port*)))

(defparameter *url-store-type* :hash)

(defparameter *psql-old-credentials*
  '(("botdb" "semaphor" nil :unix)
    ("bootsydb" "semaphor" nil :unix)
    ("shogundb" "semaphor" nil :unix)
    ("thugdb" "semaphor" nil :unix)))

(defparameter *psql-new-credentials* '("harliedb" "semaphor" nil :unix))
