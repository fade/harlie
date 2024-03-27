;;;; context.lisp

(in-package #:harlie)

(defclass bot-context ()
  ((bot-nick :initarg :bot-nick :initform nil :accessor bot-nick)
   (bot-irc-server :initarg :bot-irc-server :initform nil :accessor bot-irc-server)
   (bot-irc-channel :initarg :bot-irc-channel :initform nil :accessor bot-irc-channel)
   (bot-web-server :initarg :bot-web-server :initform nil :accessor bot-web-server)
   (bot-web-port :initarg :bot-web-port :initform nil :accessor bot-web-port)
   (bot-uri-prefix :initarg :bot-uri-prefix :initform nil :accessor bot-uri-prefix)))

(defmethod initialize-instance :after ((context bot-context) &key)
  (with-connection (psql-botdb-credentials *bot-config*)
    (let* ((context-query-head '(:select 'context-name 'irc-server 'irc-channel
                                 'web-server 'web-port 'web-uri-prefix
				 :from 'contexts))
	   (context-query (cond ((bot-nick context)
				 (append context-query-head
					 `(:where (:= (:raw "lower(context_name)")
						      ,(string-downcase (bot-nick context))))))
				((bot-web-port context)
				 (append context-query-head
					 `(:where (:= 'web-port ,(bot-web-port context)))))
				(t nil))))
      (let ((conlist (first (query (sql-compile context-query)))))
	(setf (bot-nick context) (string-downcase (first conlist)))
	(setf (bot-irc-server context) (second conlist))
	(setf (bot-irc-channel context) (third conlist))
	(setf (bot-web-server context) (fourth conlist))
	(setf (bot-web-port context) (fifth conlist))
	(setf (bot-uri-prefix context) (sixth conlist))))))

(defun chain-context (nick)
  (with-connection (psql-botdb-credentials *bot-config*)
    (query (:select 'context-id
	    :from 'contexts
	    :where (:= (:raw "lower(context_name)")
		       (string-downcase nick)))
	   :single)))

(defgeneric chain-read-context-id (context)
  (:documentation "Returns the context ID for reading the chaining DB in a given context."))

(defmethod chain-read-context-id ((context bot-context)) 
  (chain-context (bot-nick context)))

(defgeneric chain-write-context-id (context)
  (:documentation "Returns the context ID for writing to the chaining DB in a given context."))

(defmethod chain-write-context-id ((context bot-context)) 
  (chain-context (bot-nick context)))

(defgeneric url-read-context-id (context)
  (:documentation "Returns the context ID for reading the URL DB in a given context."))

(defun url-context (port)
  (with-connection (psql-botdb-credentials *bot-config*)
    (query (:select 'context-id
	    :from 'contexts
	    :where (:= 'web-port port))
	   :single)))

(defmethod url-read-context-id ((context bot-context)) 
  (url-context (bot-web-port context)))

(defgeneric url-write-context-id (context)
  (:documentation "Returns the context ID for writing to the URL DB in a given context."))

(defmethod url-write-context-id ((context bot-context))
  (url-context (bot-web-port context)))
