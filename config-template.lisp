;;;; config-template.lisp

;; With respect to David Gerrold's _When HARLIE Was One_

(in-package #:harlie)

;; this sets the logging verbosity. 
(log:config :debug)

(defparameter *harlot-config*
  (make-config :irc-server-name "irc.srh.org"
	       :irc-nickchannels '(("Harlot" ("#trinity")))
	       :irc-joins '(("irc.srh.org" (("Harlot" ("#trinity" "#triscuit")))))
	       :web-server-name "127.0.0.1"
	       :web-server-ports '(5791)
	       :url-store-type :psql
	       :psql-botdb-credentials '("botdb" "tuxedo" nil :unix)))

(defparameter *sr-4-config*
  (make-config :irc-server-name "irc.srh.org"
	       :irc-nickchannels '(("SR-4" ("#trinity")))
	       :irc-joins '(("irc.srh.org" (("SR-4" ("#trinity")))))
	       :web-server-name "localhost"
	       :web-server-ports '(5791)
	       :url-store-type :psql
	       :psql-botdb-credentials '("botdb" "fade" nil :unix)))

(defparameter *combined-config*
  (make-config :irc-server-name "irc.srh.org"
               :irc-nickchannels '(("semaphor" ("#deepsky"))
                                   ("Bootsy" ("#funkrehab"))
                                   ("shogun" ("#walled"))
                                   ("thugster" ("#wallednoc")))
	       :irc-joins '(("irc.srh.org"
			     (("semaphor" ("#deepsky"))
			      ("Bootsy" ("#funkrehab"))
			      ("shogun" ("#walled"))
			      ("thugster" ("#wallednoc")))))
               :web-server-name "coruscant.deepsky.com"
               :web-server-ports '(7080 8080 8090 9099)
               :url-store-type :psql
               :psql-botdb-credentials '("botdb" "semaphor" nil :unix)))

(defparameter *bot-config* *combined-config*)

(defparameter *twitter-auth* nil
  "if you want the bot to grab channel linked tweets and drop them in
  the headlines fetched by the URL shortener, you need to provide a
  twitter access token, here.")
