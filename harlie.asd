;;;; harlie.asd

(asdf:defsystem #:harlie
  :serial t
  :depends-on (#:sb-concurrency
	       #:bordeaux-threads
	       #:cl-irc
	       #:cl-ppcre
	       #:closure-html
	       #:cxml-stp
	       #:drakma
	       #:hunchentoot
	       #:cl-who
	       #:postmodern
	       #:local-time
	       #:split-sequence
	       #:cl-fad)
  :components ((:file "package")
	       (:file "config")
	       (:file "util")
	       (:file "url-store")
	       (:file "web-client")
	       (:file "web-server")
	       (:file "irc-client")
	       (:file "constants")
	       (:file "plugins")
	       (:file "harlie")))
