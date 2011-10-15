;;;; harlie.asd

(asdf:defsystem #:harlie
  :serial t
  :depends-on (#:cl-irc
	       #:cl-ppcre
	       #:closure-html
	       #:cxml-stp
	       #:drakma)
  :components ((:file "package")
	       (:file "harlie")))
