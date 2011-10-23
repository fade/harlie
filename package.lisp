;;;; package.lisp

(defpackage #:harlie
  (:use #:cl
	#:bordeaux-threads
	#:sb-concurrency
	#:cl-ppcre
	#:closure-html
	#:cxml-stp
	#:drakma
	#:cl-who
	#:hunchentoot
	#:irc
	#:postmodern
	#:local-time
	#:split-sequence)
  (:shadowing-import-from :closure-html :attribute :text :comment)
  (:shadowing-import-from :hunchentoot
   :host :*header-stream* :cookie-path :cookie-expires :cookie-name
   :cookie-domain :parameter-error :cookie-value)
  (:shadowing-import-from :cl-irc :connect))
