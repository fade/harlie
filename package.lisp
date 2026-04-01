;;;; package.lisp

(defpackage #:harlie
  (:use #:cl
	#:alexandria
	#:bt
        ;; #:lparallel
        #:cl+ssl
        #:cl-cron
        #:log4cl
	#:cl-ppcre
	#:closure-html
	#:cxml-stp
	#:drakma
	#:hunchentoot
	#:json
	#:local-time
	#:parse-number
	#:postmodern
  	#:split-sequence)
  
  (:shadowing-import-from :closure-html :attribute :text :comment)
  (:shadowing-import-from :hunchentoot
   :host :*header-stream* :cookie-path :cookie-expires :cookie-name
                          :cookie-domain :parameter-error :cookie-value :url-encode)
  ;; clatter-irc's connect shadows postmodern's connect
  (:shadowing-import-from :clatter-irc :connect)
  ;; (:shadowing-import-from :cl-who :with-html-output)
  (:shadowing-import-from :drakma :header-value)
  ;; (:shadowing-import-from :cl-log :log-message :make-timestamp :timestamp)
  (:shadowing-import-from :postmodern :encode-json-to-string)
  (:import-from :clatter-irc
   ;; Connection management
   #:connection
   #:make-connection
   #:disconnect
   #:connectedp
   #:send-raw
   #:connection-nick
   #:connection-server
   #:connection-port
   #:connection-tls-p
   #:connection-user-data
   #:connection-channels
   #:connection-stream
   #:connection-socket
   ;; Hook system
   #:add-hook
   #:remove-hook
   #:run-hooks
   ;; IRC commands
   #:privmsg
   #:join
   #:notice
   #:quit
   ;; Message parsing
   #:message
   #:message-command
   #:message-params
   #:message-prefix
   #:message-raw
   #:message-tags
   #:parse-prefix
   #:prefix-nick
   #:prefix-user
   #:prefix-host
   ;; Constants
   #:*default-port*
   #:*default-tls-port*)
  (:export #:run-bot
	   #:kill-bot
	   #:make-bot-connection
	   #:make-irc-client-instance-thunk
	   #:connection-state
	   #:*irc-connections*))

(local-time:set-local-time-cl-postgres-readers)
