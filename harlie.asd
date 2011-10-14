;;;; harlie.asd

(asdf:defsystem #:harlie
  :serial t
  :depends-on (#:cl-irc)
  :components ((:file "package")
	       (:file "harlie")))
