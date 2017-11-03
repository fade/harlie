;;;; harlie.asd

(asdf:defsystem #:harlie
  :serial t
  :depends-on (#:alexandria
	       #:bordeaux-threads
	       #:cl-fad
	       #:cl-irc
	       #:cl-log
	       #:cl-json
               #:jsown
	       #:cl-ppcre
	       #:cl-who
	       #:closure-html
	       #:colorize
	       #:cxml-stp
	       #:drakma
	       #:hunchentoot
	       #:jpl-queues
	       #:local-time
               #:simple-date-time
	       #:parse-number
	       #:postmodern
	       #:split-sequence
	       #:trivial-shell
	       #:trivial-timeout
	       #:yaclml)

  :components ((:file "package")
	       (:file "adaptation")
	       (:file "confobjects")
	       (:file "config")
               (:file "users")
	       (:file "context")
	       (:file "util")
	       (:file "url-store")
	       (:file "irc-client")
	       (:file "constants")
	       (:file "chainer")
	       (:file "plugins")
	       (:file "web-client")
;	       (:file "http-request")
	       (:file "web-server")
	       (:file "sleep-timers")
	       (:file "harlie")))
