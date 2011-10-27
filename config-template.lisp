;;;; config-template.lisp

;; With respect to David Gerrold's _When HARLIE Was One_

(in-package :harlie)

(defstruct config
  irc-server-name
  irc-nick
  irc-channel-names
  web-server-name
  web-server-port
  url-store-type
  psql-old-credentials
  psql-url-new-credentials
  psql-chain-credentials)

(defparameter *shogun-config*
  (make-config :irc-server-name "irc.srh.org"
	       :irc-nick "shogun"
	       :irc-channel-names '("#walled")
	       :web-server-name "coruscant.deepsky.com"
	       :web-server-port 5783
	       :url-store-type :psql
	       :psql-old-credentials '(("botdb" "semaphor" nil :unix)
				       ("bootsydb" "semaphor" nil :unix)
				       ("shogundb" "semaphor" nil :unix)
				       ("thugdb" "semaphor" nil :unix))
	       :psql-url-new-credentials '("shogundb" "semaphor" nil :unix)
	       :psql-chain-credentials '("shogundb" "semaphor" nil :unix)))

(defparameter *harlie-config*
  (make-config :irc-server-name "irc.srh.org"
	       :irc-nick "Harlie"
	       :irc-channel-names '("#trinity")
	       :web-server-name "coruscant.deepsky.com"
	       :web-server-port 5791
	       :url-store-type :psql
	       :psql-old-credentials '(("botdb" "semaphor" nil :unix)
				       ("bootsydb" "semaphor" nil :unix)
				       ("shogundb" "semaphor" nil :unix)
				       ("thugdb" "semaphor" nil :unix))
	       :psql-url-new-credentials '("harliedb" "semaphor" nil :unix)
	       :psql-chain-credentials '("botdb" "semaphor" nil :unix)))

(defparameter *harlot-config*
  (make-config :irc-server-name "irc.srh.org"
	       :irc-nick "Harlot"
	       :irc-channel-names '("#triscuit")
	       :web-server-name "coruscant.deepsky.com"
	       :web-server-port 5791
	       :url-store-type :psql
	       :psql-old-credentials '(("botdb" "tuxedo" nil :unix)
				       ("bootsydb" "tuxedo" nil :unix)
				       ("shogundb" "tuxedo" nil :unix)
				       ("thugdb" "tuxedo" nil :unix))
	       :psql-url-new-credentials '("harliedb" "tuxedo" nil :unix)
	       :psql-chain-credentials '("botdb" "tuxedo" nil :unix)))

(defparameter *bot-config* *harlie-config*)
