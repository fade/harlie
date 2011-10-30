;;;; context.lisp

(in-package :harlie)

(defclass bot-context ()
  ((bot-nick :initarg :bot-nick :initform nil :accessor bot-nick)
   (bot-irc-server :initarg :bot-irc-server :initform nil :accessor bot-irc-server)
   (bot-irc-channel :initarg :bot-irc-channel :initform nil :accessor bot-irc-channel)
   (bot-web-server :initarg :bot-web-server :initform nil :accessor bot-web-server)
   (bot-web-port :initarg :bot-web-port :initform nil :accessor bot-web-port)
   (bot-uri-prefix :initarg :bot-uri-prefix :initform nil :accessor bot-uri-prefix)))

(defmethod initialize-instance :after ((context bot-context) &key)
  (cond ((bot-nick context)
	 (with-connection (config-psql-context-credentials *bot-config*)
	   (let ((conlist
		   (first (query (:select 'irc-server 'irc-channel 'web-server
					  'web-port 'web-uri-prefix
				  :from 'contexts
				  :where (:= (:raw "lower(context_name)") (string-downcase (bot-nick context))))))
))
	     (setf (bot-nick context) (string-downcase (bot-nick context)))
	     (setf (bot-irc-server context) (first conlist))
	     (setf (bot-irc-channel context) (second conlist))
	     (setf (bot-web-server context) (third conlist))
	     (setf (bot-web-port context) (fourth conlist))
	     (setf (bot-uri-prefix context) (fifth conlist)))))
	((bot-web-port context)
	 (with-connection (config-psql-context-credentials *bot-config*)
	   (let ((conlist
		   (query (:select 'context-name 'irc-server 'irc-channel 'web-server
				   'web-uri-prefix
			   :from 'contexts
			   :where (:= 'web-port (bot-web-port context))))))
	     (setf (bot-nick context) (string-downcase (first conlist)))
	     (setf (bot-irc-server context) (second conlist))
	     (setf (bot-irc-channel context) (third conlist))
	     (setf (bot-web-server context) (fourth conlist))
	     (setf (bot-web-port context) (fifth conlist))
	     (setf (bot-uri-prefix context) (sixth conlist)))))
	(t nil)))

(defgeneric chain-read-credentials (context)
  (:documentation "Returns the database credentials to read the chaining DB in a given context."))

(defmethod chain-read-credentials ((context bot-context))
  (declare (ignore context))
  (config-psql-chain-credentials *bot-config*))

(defgeneric chain-read-context-id (context)
  (:documentation "Returns the context ID for reading the chaining DB in a given context."))

(defmethod chain-read-context-id ((context bot-context)) 
  (with-connection (config-psql-context-credentials *bot-config*)
    (query (:select 'context-id
	    :from 'contexts
	    :where (:= (:raw "lower(context_name)")
		       (string-downcase (bot-nick context))))
	   :single)))

(defgeneric chain-write-credentials (context)
  (:documentation "Returns the database credentials to write to the chaining DB in a given context."))

(defmethod chain-write-credentials ((context bot-context))
  (declare (ignore context))
  (config-psql-chain-credentials *bot-config*))

(defgeneric chain-write-context-id (context)
  (:documentation "Returns the context ID for writing to the chaining DB in a given context."))

(defmethod chain-write-context-id ((context bot-context)) 
  (with-connection (config-psql-context-credentials *bot-config*)
    (query (:select 'context-id
	    :from 'contexts
	    :where (:= (:raw "lower(context_name)")
		       (string-downcase (bot-nick context))))
	   :single)))

(defgeneric url-read-credentials (context)
  (:documentation "Returns the database credentials to read the URL DB in a given context."))

(defmethod url-read-credentials ((context bot-context))
  (config-psql-url-new-credentials *bot-config*))

(defgeneric url-read-context-id (context)
  (:documentation "Returns the context ID for reading the URL DB in a given context."))

(defmethod url-read-context-id ((context bot-context)) 
  (with-connection (config-psql-context-credentials *bot-config*)
    (query (:select 'context-id
	    :from 'contexts
	    :where (:= 'web-port (bot-web-port context)))
	   :single)))

(defgeneric url-write-credentials (context)
  (:documentation "Returns the database credentials to write to the chaining DB in a given context."))

(defmethod url-write-credentials ((context bot-context))
  (config-psql-url-new-credentials *bot-config*))

(defgeneric url-write-context-id (context)
  (:documentation "Returns the context ID for writing to the chaining DB in a given context."))

(defmethod url-write-context-id ((context bot-context))
  (with-connection (config-psql-context-credentials *bot-config*)
    (query (:select 'context-id
	    :from 'contexts
	    :where (:= 'web-port (bot-web-port context)))
	   :single)))
