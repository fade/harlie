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
	       #:local-time)
  :components ((:file "package")
	       (:file "config")
	       (:file "url-store")
	       (:file "plugins")
	       (:file "util")
	       (:file "harlie")))
