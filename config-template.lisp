;;;; config-template.lisp

;; With respect to David Gerrold's _When HARLIE Was One_

(in-package #:harlie)

(defparameter *threads* 20 "The threads to use to check stored URLs in parallel.")

;; the phrase the bot utters to self-identify to other bots (and people). This must be
;; coordinated with other bot herders.

(defparameter *ignore-phrase* "NOTIFY:: Help, I'm a bot!"
  "Bot emits this phrase on channel join to announce its botness.")

;; this sets the logging verbosity. 
(log:config :debug)

;; database access credentials. CHANGE THIS PER YOUR SITE
(defparameter *bot-database-credentials*
  '("botdb" "your-username" nil :unix))

(defparameter *binary-url-suffixes*
  (list ".png" ".jpg" ".mov" "mkv" ".webv" ".avi" ".mp4" ".atom" ".rss" ".doc" ".xls" ".odt")
  "a list of suffixes for which we will not do an http get request to get a title element.")

(defparameter *user-agents* '("Mozilla/5.0 (X11; Linux x86_64; rv:69.0) Gecko/20100101 Firefox/69.0"
                              "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/76.0.3809.100 Safari/537.36"
                              "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/76.0.3809.100 Safari/537.36"
                              "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/12.1.2 Safari/605.1.15"
                              "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/75.0.3770.142 Safari/537.36"))

;; these are exmaples.
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
               :web-server-name "deepsky.com"
               :web-server-ports '(7080 8080 8090 9099)
               :url-store-type :psql
               ;; :psql-botdb-credentials '("botdb" "semaphor" nil :unix)
               :psql-botdb-credentials *bot-database-credentials*))

(defparameter *bot-config* *combined-config*)

(defparameter *twitter-auth* nil
  "if you want the bot to grab channel linked tweets and drop them in
  the headlines fetched by the URL shortener, you need to provide a
  twitter access token, here.")
