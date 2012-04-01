;;;; package.lisp

(defpackage #:harlie
  (:use #:cl
	#:sb-concurrency
	#:alexandria
	#:bt
	#:cl-log
	#:cl-ppcre
	#:cl-who
	#:closure-html
	#:cxml-stp
	#:drakma
	#:hunchentoot
	#:irc
	#:json
	#:local-time
	#:parse-number
	#:postmodern
	#:split-sequence)
  (:shadowing-import-from :closure-html :attribute :text :comment)
  (:shadowing-import-from :hunchentoot
   :host :*header-stream* :cookie-path :cookie-expires :cookie-name
   :cookie-domain :parameter-error :cookie-value :url-encode)
  (:shadowing-import-from :cl-irc :connect)
  (:shadowing-import-from :cl-who :with-html-output)
  (:shadowing-import-from :drakma :header-value)
  (:shadowing-import-from :cl-log :log-message :make-timestamp :timestamp)
  (:export #:run-bot
	   #:kill-bot))
