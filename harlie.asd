;;;; harlie.asd

(asdf:defsystem #:harlie
  :serial t
  :depends-on (#:sb-concurrency
	       #:bordeaux-threads
	       #:cl-fad
	       #:cl-irc
	       #:cl-log
	       #:cl-ppcre
	       #:cl-who
	       #:closure-html
	       #:colorize
	       #:cxml-stp
	       #:drakma
	       #:hunchentoot
	       #:local-time
	       #:parse-number
	       #:postmodern
	       #:split-sequence
	       #:trivial-shell
	       #:trivial-timeout
	       #:yaclml)

  :components ((:file "package")
	       (:file "config")
	       (:file "context")
	       (:file "util")
	       (:file "url-store")
	       (:file "irc-client")
	       (:file "constants")
	       (:file "chainer")
	       (:file "plugins")
	       (:file "web-client")
	       (:file "http-request")
	       (:file "web-server")
	       (:file "harlie")))
