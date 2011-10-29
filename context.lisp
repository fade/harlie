;;;; context.lisp

(in-package :harlie)

(defclass bot-context ()
  ((bot-nick :initarg :bot-nick :initform nil :accessor bot-nick)
   (bot-irc-server :initarg :bot-irc-server :initform nil :accessor bot-irc-server)
   (bot-irc-channel :initarg :bot-irc-channel :initform nil :accessor bot-irc-channel)
   (bot-web-server :initarg :bot-web-server :initform nil :accessor bot-web-server)
   (bot-web-port :initarg :bot-web-port :initform nil :accessor bot-web-port)
   (bot-uri-prefix :initarg :bot-uri-prefix :initform nil :accessor bot-uri-prefix)))

(defgeneric chain-read-credentials (context)
  (:documentation "Returns the database credentials to read the chaining DB in a given context."))

(defmethod chain-read-credentials ((context bot-context))
  (declare (ignore context))
  (config-psql-chain-credentials *bot-config*))

(defgeneric chain-read-context-id (context)
  (:documentation "Returns the context ID for reading the chaining DB in a given context."))

(defmethod chain-read-context-id (context)
  (with-connection (config-psql-context-credentials *bot-config*)
    (query (:select 'context-id
	    :from 'contexts
	    :where (:= 'context-name
		       (string-downcase (bot-nick context))))
	   :single)))

(defgeneric chain-write-credentials (context)
  (:documentation "Returns the database credentials to write to the chaining DB in a given context."))

(defmethod chain-write-credentials ((context bot-context))
  (declare (ignore context))
  (config-psql-chain-credentials *bot-config*))

(defgeneric chain-write-context-id (context)
  (:documentation "Returns the context ID for writing to the chaining DB in a given context."))

(defmethod chain-write-context-id (context)
  (with-connection (config-psql-context-credentials *bot-config*)
    (query (:select 'context-id
	    :from 'contexts
	    :where (:= 'context-name
		       (string-downcase (bot-nick context))))
	   :single)))
