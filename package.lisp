;;;; package.lisp

(defpackage #:harlie
  (:use #:cl
	#:cl-ppcre
	#:closure-html
	#:cxml-stp
	#:drakma
	#:irc
	#:sb-thread)
  (:shadowing-import-from :closure-html :attribute :text :comment)
  )
