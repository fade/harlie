;;;; harlie.asd

(asdf:defsystem #:harlie
  :serial t
  :in-order-to ((asdf:test-op (asdf:test-op #:harlie/test/all)))
  :depends-on (#:alexandria
	       #:bordeaux-threads
               #:lparallel
               #:cl-cron
	       #:cl-fad
               #:cl-sasl
               #:cl+ssl
	       #:clatter-irc
               #:log4cl
	       #:cl-json
               #:com.inuoe.jzon
               #:cl-csv
               #:jsown
	       #:cl-ppcre
               #:cl-interpol
               #:spinneret
	       ;; #:cl-who ;TODO: remove cl-who from asd
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
               #:str
               #:cl-strings
               #:slynk)

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
               (:file "init-first-run")
	       (:file "harlie")))

(asdf:defsystem #:harlie/test/fake-irc-server
  :depends-on (#:harlie #:bordeaux-threads #:usocket #:cl-ppcre)
  :components ((:file "test/fake-irc-server")))

(asdf:defsystem #:harlie/test/nickserv-flow
  :depends-on (#:harlie #:harlie/test/fake-irc-server #:parachute)
  :components ((:file "test/nickserv-flow")))

(asdf:defsystem #:harlie/test/unit
  :depends-on (#:harlie #:parachute)
  :serial t
  :components ((:module "test/unit"
                :serial t
                :components ((:file "urls")
                             (:file "triggers")
                             (:file "config")))))

(asdf:defsystem #:harlie/test/db
  :depends-on (#:harlie #:parachute #:postmodern)
  :serial t
  :components ((:module "test/db"
                :serial t
                :components ((:file "package")
                             (:file "contexts")
                             (:file "users")))))

(asdf:defsystem #:harlie/test/all
  :depends-on (#:harlie/test/fake-irc-server
               #:harlie/test/nickserv-flow
               #:harlie/test/unit
               #:harlie/test/db
               #:parachute)
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :parachute :test
                               '(:harlie/test/nickserv-flow
                                 :harlie/test/unit/urls
                                 :harlie/test/unit/triggers
                                 :harlie/test/unit/config
                                 :harlie/test/db/contexts
                                 :harlie/test/db/users))))
