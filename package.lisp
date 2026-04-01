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
  ;; clatter-irc's connect and disconnect shadow postmodern's
  (:shadowing-import-from :clatter-irc :connect :disconnect)
  ;; (:shadowing-import-from :cl-who :with-html-output)
  (:shadowing-import-from :drakma :header-value)
  ;; (:shadowing-import-from :cl-log :log-message :make-timestamp :timestamp)
  (:shadowing-import-from :postmodern :encode-json-to-string)
  (:import-from :clatter-irc
   ;; Connection management
   #:connection
   #:make-connection
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
   ;; Hook names (must be same symbols clatter-irc uses in run-hooks)
   #:on-connect
   #:on-disconnect
   #:on-privmsg
   #:on-notice
   #:on-join
   #:on-part
   #:on-quit
   #:on-nick
   #:on-numeric
   #:on-ctcp
   ;; Read thread accessor (for join-thread in thunk)
   #:connection-read-thread
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
   #:*default-tls-port*
   ;; Channel tracking (NOT channel-name or channel-user — conflicts with DAO classes)
   #:channel
   #:find-channel
   #:ensure-channel
   #:joined-channels
   #:joined-channel-p
   #:connection-channel-class)
  (:export #:run-bot
	   #:kill-bot
	   #:make-bot-connection
	   #:make-irc-client-instance-thunk
	   #:bot-state
	   #:*irc-connections*))

(local-time:set-local-time-cl-postgres-readers)
