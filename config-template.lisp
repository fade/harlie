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

;; ---------------------------------------------------------------------------
;; Bot configuration
;;
;; Each IRC connection is described by a connection-spec.  One connection-spec
;; = one nick on one channel on one server, with its own URL-shortener web
;; port.  Collect them all in a make-config :connections list.
;;
;; make-connection-spec fields:
;;   :server   — IRC server hostname                           (required)
;;   :ssl      — t for TLS (port 6697), nil for plain (6667)  (required)
;;   :nick     — IRC nickname                                  (required)
;;   :channel  — channel to join, including the # sign        (required)
;;   :web-port — port for the URL-shortener HTTP server       (required)
;;   :channel-key       — channel password, if any            (optional)
;;   :nickserv-password — NickServ IDENTIFY password, if any  (optional)
;;   :public-port      — public port for URLs behind a reverse proxy (optional)
;;                        e.g. 443 when Caddy/nginx terminates TLS
;;
;; On every startup, load-contexts syncs the database contexts table from
;; this list automatically.  No manual SQL is needed when adding channels.
;;
;; TLS for the URL-shortener web server is optional.  Pass :tls-cert-file
;; and :tls-key-file to make-config with paths to a PEM certificate and its
;; matching private key.  If both files exist and are readable at startup,
;; the web servers come up as HTTPS and shortened URLs are emitted with an
;; https:// scheme; otherwise the bot falls back to plain HTTP and http://
;; URLs.  Leave the slots unset to run without TLS.
;; ---------------------------------------------------------------------------

;; Minimal example: one bot, one channel.
(defparameter *minimal-config*
  (make-config
   :db-credentials  *bot-database-credentials*
   :web-server-name "localhost"
   :connections
   (list
    (make-connection-spec
     :server "irc.libera.chat" :ssl t
     :nick "YourBotNick" :channel "#yourchannel"
     :web-port 8888))))

;; Multi-connection example: nicks on both Libera.Chat and SRH.org,
;; demonstrating optional fields (NickServ password, channel key).
(defparameter *multi-config*
  (make-config
   :db-credentials  *bot-database-credentials*
   :web-server-name "yourhost.example.com"
   :connections
   (list
    ;; Libera.Chat — registered nick with NickServ password
    (make-connection-spec
     :server "irc.libera.chat" :ssl t
     :nick "YourBotNick" :channel "#yourchannel"
     :nickserv-password "your-nickserv-password"
     :web-port 8888)
    ;; Libera.Chat — second nick, behind Caddy reverse proxy (public HTTPS)
    (make-connection-spec
     :server "irc.libera.chat" :ssl t
     :nick "AnotherNick" :channel "#anotherchannel"
     :web-port 8889
     :public-port 443)
    ;; SRH.org — open channel
    (make-connection-spec
     :server "irc.srh.org" :ssl t
     :nick "YourBotNick" :channel "#srh-channel"
     :web-port 8890)
    ;; SRH.org — password-protected channel
    (make-connection-spec
     :server "irc.srh.org" :ssl t
     :nick "AnotherNick" :channel "#private" :channel-key "channelpassword"
     :web-port 8891))))

;; Set *bot-config* to whichever config you want active.
;; Start with *minimal-config* and expand from there.
(defparameter *bot-config* *minimal-config*)

(defparameter *twitter-auth* nil
  "if you want the bot to grab channel linked tweets and drop them in
  the headlines fetched by the URL shortener, you need to provide a
  twitter access token, here.")
