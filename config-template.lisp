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

;; where the hell is this system located?
;; (uiop/os:getcwd)
(defparameter *here-db* (asdf:system-relative-pathname "harlie" "database/"))

(defparameter *trig* (alexandria:shuffle (str:split-omit-nulls #\Newline
                                                               (rutils:slurp (merge-pathnames
                                                                              (asdf:system-relative-pathname :harlie "constants/") "initial-triggerlist.txt"))))
  "a list of common english words to seed the bot's triggerlist before it
has a chaining database. shuffled for freshness.")

(defparameter *binary-url-suffixes*
  (list ".png" ".jpg" ".mov" ".mkv" ".webv" ".avi" ".mp4" ".atom" ".rss" ".doc" ".xls" ".odt")
  "a list of suffixes for which we will not do an http get request to get a title element.")

(defparameter *user-agents* '("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/42.0.2311.135 Safari/537.36 Edge/12.246"
                              "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_2) AppleWebKit/601.3.9 (KHTML, like Gecko) Version/9.0.2 Safari/601.3.9"
                              "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36"
                              "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:15.0) Gecko/20100101 Firefox/15.0.1"))

;; these are examples.
(defparameter *harlot-config*
  (make-config
   :db-credentials  '("botdb" "tuxedo" nil :unix)
   :web-server-name "127.0.0.1"
   :connections
   (list
    (make-connection-spec
     :server "irc.srh.org" :ssl nil
     :nick "Harlot" :channel "#trinity"
     :web-port 5791))))

(defparameter *sr-4-config*
  (make-config
   :db-credentials  '("botdb" "fade" nil :unix)
   :web-server-name "localhost"
   :connections
   (list
    (make-connection-spec
     :server "irc.srh.org" :ssl nil
     :nick "SR-4" :channel "#trinity"
     :web-port 5791))))

(defparameter *combined-config*
  (make-config
   :db-credentials  *bot-database-credentials*
   :web-server-name "deepsky.com"
   :connections
   (list
    (make-connection-spec
     :server "irc.srh.org" :ssl nil
     :nick "semaphor" :channel "#deepsky"
     :web-port 7080)
    (make-connection-spec
     :server "irc.srh.org" :ssl nil
     :nick "Bootsy" :channel "#funkrehab"
     :web-port 8080)
    (make-connection-spec
     :server "irc.srh.org" :ssl nil
     :nick "shogun" :channel "#walled"
     :web-port 8090)
    (make-connection-spec
     :server "irc.srh.org" :ssl nil
     :nick "thugster" :channel "#wallednoc"
     :web-port 9099))))

(defparameter *bot-config* *combined-config*)

(defparameter *twitter-auth* nil
  "if you want the bot to grab channel linked tweets and drop them in
  the headlines fetched by the URL shortener, you need to provide a
  twitter access token, here.")
