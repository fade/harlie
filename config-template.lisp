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


