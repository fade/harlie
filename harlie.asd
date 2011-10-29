;;;; harlie.asd

(asdf:defsystem #:harlie
  :serial t
  :depends-on (#:sb-concurrency
	       #:bordeaux-threads
	       #:cl-irc
	       #:cl-ppcre
	       #:closure-html
	       #:cxml-stp
	       #:yaclml
	       #:drakma
	       #:hunchentoot
	       #:cl-who
	       #:postmodern
	       #:local-time
	       #:split-sequence
	       #:cl-fad
	       #:parse-number)

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
