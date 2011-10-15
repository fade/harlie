;;;; package.lisp

(defpackage #:harlie
  (:use #:cl
	#:bordeaux-threads
	#:cl-ppcre
	#:closure-html
	#:cxml-stp
	#:drakma
	#:hunchentoot
	#:irc)
  (:shadowing-import-from :closure-html :attribute :text :comment)
  (:shadowing-import-from :hunchentoot
   :host :*header-stream* :cookie-path :cookie-expires :cookie-name
   :cookie-domain :parameter-error :cookie-value))
