;;;; harlie.asd

(asdf:defsystem #:harlie
  :serial t
  :depends-on (#:bordeaux-threads
	       #:cl-irc
	       #:cl-ppcre
	       #:closure-html
	       #:cxml-stp
	       #:drakma
	       #:hunchentoot
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
	       (:file "constants")
	       (:file "plugins")
	       (:file "harlie")))
