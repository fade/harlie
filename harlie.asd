;;;; harlie.asd

(asdf:defsystem #:harlie
  :serial t
  :depends-on (#:alexandria
	       #:bordeaux-threads
               #:lparallel
               #:cl-cron
	       #:cl-fad
	       #:cl-irc
               #:log4cl
	       #:cl-json
               #:com.inuoe.jzon
               #:cl-csv
               #:jsown
	       #:cl-ppcre
               #:cl-interpol
               #:spinneret
	       #:cl-who
               ;; webscraping
	       #:closure-html
               #:dexador
               #:plump
               #:lquery
	       #:cxml-stp
               ;; /webscraping
               ;; web server and client
	       #:colorize
	       #:drakma ;; TODO remove me in favour of dexador.
	       #:hunchentoot
               ;; /web server and client
	       #:jpl-queues
	       #:local-time
               #:cl-postgres+local-time
               #:simple-date-time
               #:net-telent-date
	       #:parse-number
	       #:postmodern
               #:puri
               #:rutils
	       #:split-sequence
	       #:trivial-shell
	       #:trivial-timeout
	       #:yaclml
               #:str)

  :components ((:file "package")
	       (:file "adaptation")
	       (:file "confobjects")
	       (:file "config")
               (:file "cron")

               (:file "users")

	       (:file "context")
	       (:file "util")
	       (:file "url-store")
	       (:file "irc-client")

	       (:file "constants")
	       (:file "chainer")
	       (:file "plugins")
	       (:file "web-client")
	       (:file "web-server")
	       (:file "sleep-timers")
	       (:file "harlie")))
