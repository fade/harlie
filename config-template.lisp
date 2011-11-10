;;;; config-template.lisp

;; With respect to David Gerrold's _When HARLIE Was One_

(in-package #:harlie)

(defstruct config
  irc-server-name
  irc-nick ;Deprecated
  irc-channel-names ;Deprecated
  irc-nickchannels
  web-server-name
  web-server-port ; Deprecated
  web-server-ports
  url-store-type
  psql-old-credentials
  psql-url-new-credentials
  psql-chain-credentials
  psql-context-credentials)

(defparameter *harlie-config*
  (make-config :irc-server-name "irc.srh.org"
	       :irc-nickchannels '(("Harlie" ("#trinity")))
	       :irc-channel-names '("#trinity")
	       :web-server-name "coruscant.deepsky.com"
	       :web-server-port '(5791)
	       :url-store-type :psql
	       :psql-old-credentials '(("botdb" "semaphor" nil :unix)
				       ("bootsydb" "semaphor" nil :unix)
				       ("shogundb" "semaphor" nil :unix)
				       ("thugdb" "semaphor" nil :unix))
	       :psql-url-new-credentials '("harliedb" "semaphor" nil :unix)
	       :psql-chain-credentials '("botdb" "semaphor" nil :unix)
	       :psql-context-credentials '("botdb" "semaphor" nil :unix)))

(defparameter *harlot-config*
  (make-config :irc-server-name "irc.srh.org"
	       :irc-nickchannels '(("Harlot" ("#trinity" "#triscuit")))
	       :web-server-name "127.0.0.1"
	       :web-server-ports '(5791)
	       :url-store-type :psql
	       :psql-old-credentials '(("botdb" "tuxedo" nil :unix))
	       :psql-url-new-credentials '("botdb" "tuxedo" nil :unix)
	       :psql-chain-credentials '("botdb" "tuxedo" nil :unix)
	       :psql-context-credentials '("botdb" "tuxedo" nil :unix)))

(defparameter *sr-4-config*
  (make-config :irc-server-name "irc.srh.org"
	       :irc-nickchannels '(("SR-4" ("#trinity")))
	       :web-server-name "localhost"
	       :web-server-ports '(5791)
	       :url-store-type :psql
	       :psql-old-credentials '(("botdb" "fade" nil :unix))
	       :psql-url-new-credentials '("botdb" "fade" nil :unix)
	       :psql-chain-credentials '("botdb" "fade" nil :unix)
	       :psql-context-credentials '("botdb" "fade" nil :unix)))

(defparameter *combined-config*
  (make-config :irc-server-name "irc.srh.org"
               :irc-nickchannels '(("semaphor" ("#deepsky"))
                                   ("Bootsy" ("#funkrehab"))
                                   ("shogun" ("#walled"))
                                   ("thugster" ("#wallednoc")))
               :web-server-name "coruscant.deepsky.com"
               :web-server-ports '(7080 8080 8090 9099)
               :url-store-type :psql
               :psql-old-credentials '(("botdb" "semaphor" nil :unix))
               :psql-url-new-credentials '("botdb" "semaphor" nil :unix)
               :psql-chain-credentials '("botdb" "semaphor" nil :unix)
               :psql-context-credentials '("botdb" "semaphor" nil :unix)))

(defparameter *bot-config* *combined-config*)
