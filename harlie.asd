;;;; harlie.asd

(asdf:defsystem #:harlie
  :serial t
  :depends-on (#:bordeaux-threads
	       #:cl-irc
	       #:cl-ppcre
	       #:closure-html
	       #:cxml-stp
	       #:drakma
	       #:hunchentoot)
  :components ((:file "package")
	       (:file "harlie")))
