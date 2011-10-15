;;;; harlie.asd

(asdf:defsystem #:harlie
  :serial t
  :depends-on (#:cl-irc
	       #:cl-ppcre)
  :components ((:file "package")
	       (:file "harlie")))
