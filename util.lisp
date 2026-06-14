;;;; util.lisp

(in-package #:harlie)

(defun string-strip-surrounding-whitespace (str)
  "Strip whitespace characters from beginning and end of a string."
  (string-trim '(#\Space #\Newline #\Return #\Tab) str))

(defun strip-spaces (strings)
  "remove book-end whitespace from a string or list of strings."
  (cond
    ((listp strings) (mapcar #'string-strip-surrounding-whitespace strings))
    (t (string-strip-surrounding-whitespace strings))))

(defun cleanup-title (title)
  "Remove extraneous whitespace characters from within and around a string.
   Specifically: split the string on newlines; strip leading and trailing
   whitespace from each substring; drop empty substrings; compose a new string
   from the remaining substrings, with single-space separators."
  (if title
      (format nil "~{~A~^ ~}"
	      (remove-if #'(lambda (s)
			     (string= "" s))
			 (strip-spaces
			  (split-sequence #\Newline title))))
      nil))

(defun float-as-string-p (fstring)
  "Is fstring a floating point number encoded as a string?"
  (let ((scanner (create-scanner "^[0-9]*([.][0-9]*)?$")))
    (cl-ppcre:scan scanner fstring)))

(defun break-on-no-break-space (zert)
  "break up a string or list of strings by #\NO-BREAK_SPACE; these
   types of string are returned by our find-forex function as encoded
   at xe.com."
  (if (listp zert)
      (loop for string in zert
	    :collect (split-sequence:split-sequence #\NO-BREAK_SPACE string))
      (split-sequence:split-sequence #\NO-BREAK_SPACE zert)))

;;;============================================================================
;;; stock market stuff
;;;============================================================================

(defclass stock ()
  ((stock-name
    :initarg :stock-name
    :initform (error "A stock object must have a name. Please supply one.")
    :accessor stock-name)
   (stock-freshness
    :initarg :stock-freshness
    :initform nil
    :accessor stock-freshness)
   (stock-open
    :initarg :stock-open
    :initform (error "Must supply an opening price.")
    :accessor stock-open)
   (stock-high
    :initarg :stock-high
    :initform (error "Must supply a high price for the day.")
    :accessor stock-high)
   (stock-low
    :initarg :stock-low
    :initform (error "Must supply a low price for the day.")
    :accessor stock-low)
   (stock-close
    :initarg :stock-close
    :initform nil
    :accessor stock-close)
   (stock-volume
    :initarg :stock-volume
    :initform nil
    :accessor stock-volume)))

(defun jget (obj string-thing)
  "jsown is the worst name ever."
  (jsown:val obj string-thing))

(defun make-stock (name &key (function "TIME_SERIES_DAILY") (when (date-time:now)))
  (handler-case
      (let* ((raw-data (get-stock-values name :function function))
             (tradedays (list "Mon" "Tue" "Wed" "Thu" "Fri"))
             (rundate (simple-date-time:YYYY-MM-DD when))
             (stock-info ;; (jget (jget raw-data "Time Series (Daily)") rundate)
                         (cond
                           ((find (simple-date-time:day-name-of when) tradedays :test #'string-equal)
                            (jget (jget raw-data "Time Series (Daily)") rundate))
                           (t (error "Market is closed today.")))
                         )
             (metadata (jget raw-data "Meta Data"))
             (name (jget metadata "2. Symbol"))
             (freshness (date:parse-time (jget metadata "3. Last Refreshed")))
             (open (jget stock-info "1. open"))
             (high (jget stock-info "2. high"))
             (low  (jget stock-info "3. low"))
             (close (jget stock-info "4. close"))
             (volume (jget stock-info "5. volume")))
        
        (make-instance 'stock
                       :stock-name name
                       :stock-freshness freshness
                       :stock-open (parse-number open)
                       :stock-high (parse-number high)
                       :stock-low (parse-number low)
                       :stock-close (parse-number close)
                       :stock-volume (parse-number volume)))
    (error (se)
      ;;(declare (ignorable se))
      ;; (break)
      (format t "Error: ~A~2%" se)
      )))

;; (jsown:val  (jsown:val (get-stock-values "IBM" :function "TIME_SERIES_INTRADAY") "Time Series (1min)") "2017-11-02 15:00:00")

;; (jdown:val  (jsown:val (get-stock-values "IBM") "Time Series (Daily)") (simple-date-time:YYYY-MM-DD (date-time:now)))

(defun get-stock-values (stock &key (function "TIME_SERIES_DAILY"))
  "take a stock symbol, look it up using the alphavantage.co api, and
  return a list of the values encoded in the resulting JSON object."
  (let* ((data-source (cond
                        ((string-equal function "TIME_SERIES_DAILY_ADJUSTED")
                         (format nil "~A~A~A~A~A"
                                 "https://www.alphavantage.co/query?function="
                                 function
                                 "&symbol="
                                 stock
                                 ;; "&outputsize=full"
                                 "&apikey=JMRFD5OZA2QQGJKU"))
                        ((string-equal function "TIME_SERIES_INTRADAY")
                         (format nil "~A~A~A~A~A~A"
                                 "https://www.alphavantage.co/query?function="
                                 function
                                 "&symbol="
                                 stock
                                 "&interval=1min"
                                 "&apikey=JMRFD5OZA2QQGJKU"))
                        (t
                         (format nil "~A~A~A~A~A"
                                 "https://www.alphavantage.co/query?function="
                                 function
                                 "&symbol="
                                 stock
                                 ;; "&outputsize=full"
                                 "&apikey=JMRFD5OZA2QQGJKU"))))
         (quote (strip-spaces
                 (flexi-streams:octets-to-string
                  (drakma:http-request
                   data-source)
                  :external-format :utf-8))))
    ;; (format t "~A"data-source)
    (assert quote)
    (jsown:parse quote)))

(defun make-url-prefix (server-name server-port)
  "Compose the portion of an URL encoding the server name and server port.
The scheme (http vs https) follows *WEB-SCHEME*, which START-WEB-SERVERS
sets at startup based on whether a usable TLS certificate is configured.
The port must always appear in the emitted prefix unless it is the scheme's
default port, because each connection-spec runs its own hunchentoot acceptor
on its own port and the port is the only thing that distinguishes one
channel's URL-shortener context from another when several connections share
a single web-server-name."
  (let* ((https-p (or (eq *web-scheme* :https) (eql server-port 443)))
         (scheme (if https-p "https" "http"))
         (default-port (if https-p 443 80)))
    (cond
      ((or (null server-port) (eql default-port server-port))
       (format nil "~A://~A/" scheme server-name))
      (t
       (format nil "~A://~A:~A/" scheme server-name server-port)))))

(defun make-pathname-in-homedir (fname)
  "Return a pathname relative to the user's home directory."
  (merge-pathnames
   fname
   (make-pathname :directory
		  (pathname-directory
		   (user-homedir-pathname)))))

(defun make-pathname-in-lisp-subdir (fname)
  "Return a pathname relative to the Lisp source code subtree in the user's home directory."
  (merge-pathnames
   fname
   (make-pathname-in-homedir "SourceCode/lisp/")))

(defun create-caseless-scanner (s)
  (create-scanner s :case-insensitive-mode t))

(defun scan-to-substrings (rx s)
  (multiple-value-bind (whole parts) (scan-to-strings rx s)
    (declare (ignore whole))
    parts))

(defun timestamp-diff (t1 t2)
  "Find the difference, in seconds, between two timestamps."
  (abs (- (timestamp-to-universal t1) (timestamp-to-universal t2))))

(defun unix-pathstring-from-pathname (pn)
  (let* ((pnt (pathname-type pn))
	 (extension (if pnt (format nil ".~A" pnt) ""))
	 (pnn (pathname-name pn))
	 (pnd (cdr (pathname-directory pn)))
	 (pc (if pnn
		 (append pnd (list pnn))
		 pnd)))
    (format nil "/~{~A~^/~}~A" pc extension)))

(defun de-utm-url (url)
  (let ((utm-index (scan "[&?][uU][tT][mM]_|[&?][mM][bB][iI][dD]" url)))
    (if utm-index
	(subseq url 0 utm-index)
	url)))

;;; ---- Personality knobs -------------------------------------------------
;;
;; Tunable at runtime (e.g. from the Slynk REPL) to make the bot more or
;; less chatty and sarcastic without rebuilding.

(defparameter *spontaneous-reply-chance* 0.08
  "Probability [0.0-1.0] that the bot fires an unprompted Markov reply to a
message containing no trigger word.  0.0 disables spontaneous chatter.")

(defparameter *snark-chance* 0.10
  "Probability [0.0-1.0] that the bot tags a Markov reply with a sarcastic
aside drawn from *SARCASM*.  0.0 disables the snark layer.")

(defparameter *sarcasm*
  '("But what do I know, I'm just a bot."
    "Riveting stuff, truly."
    "I'll alert the media."
    "Bold of you to assume I care."
    "Groundbreaking."
    "Cool story."
    "Try to contain your excitement."
    "Sure, let's go with that."
    "I'm thrilled for you. Genuinely."
    "Noted, and immediately forgotten."
    "Fascinating. Next."
    "You're a regular Oscar Wilde."
    "Hold the front page."
    "Be still my beating CPU."
    "Wow. Just... wow."
    "Astonishing insight, as always.")
  "A pool of dry one-liners the bot occasionally appends to its chatter.")

(defun maybe-snarkify (text)
  "Occasionally append a sarcastic aside to TEXT, governed by *SNARK-CHANCE*."
  (if (and *sarcasm* (< (random 1.0) *snark-chance*))
      (format nil "~A  ~A" text (random-elt *sarcasm*))
      text))

;;; ---- Shared web page styling -------------------------------------------
;;
;; Every bot-served HTML page shares one cohesive look: a dark "terminal"
;; theme layered over Pico CSS, with a phosphor-green accent, monospace
;; headings, a sticky nav linking the pages together, and a footer.

(defparameter +bot-page-css+
  "
:root{
  --accent:#3ddc84; --accent-soft:rgba(61,220,132,.12);
  --ink:#e6edf3; --muted:#8b98a5; --bg:#0b0e13; --panel:#121821;
  --border:rgba(255,255,255,.07);
  --pico-font-family-sans-serif:'IBM Plex Sans',system-ui,sans-serif;
  --pico-font-family-monospace:'JetBrains Mono',ui-monospace,monospace;
}
:root:not([data-theme=light]),[data-theme=dark]{
  --pico-primary:var(--accent); --pico-primary-hover:#5cead0;
  --pico-primary-focus:var(--accent-soft);
  --pico-background-color:var(--bg); --pico-card-background-color:var(--panel);
  --pico-color:var(--ink); --pico-h1-color:var(--ink);
  --pico-h2-color:var(--ink); --pico-h3-color:var(--ink);
  --pico-muted-color:var(--muted);
}
body{
  font-family:var(--pico-font-family-sans-serif); min-height:100vh;
  background:
    radial-gradient(900px 500px at 78% -8%,var(--accent-soft),transparent 55%),
    radial-gradient(700px 400px at 0% 0%,rgba(80,140,255,.06),transparent 50%),
    var(--bg);
  background-attachment:fixed;
}
.site-head{
  position:sticky; top:0; z-index:20;
  background:color-mix(in srgb,var(--bg) 78%,transparent);
  -webkit-backdrop-filter:blur(10px); backdrop-filter:blur(10px);
  border-bottom:1px solid var(--border);
}
.site-head>.container{
  display:flex; align-items:center; justify-content:space-between;
  padding-block:.55rem;
}
.brand{
  font-family:var(--pico-font-family-monospace); font-weight:700;
  font-size:1.05rem; letter-spacing:.02em; color:var(--accent);
  text-decoration:none; display:inline-flex; align-items:center; gap:.15rem;
}
.cursor{ color:var(--accent); animation:blink 1.1s steps(2,start) infinite; }
@keyframes blink{ 50%{ opacity:.15; } }
.site-nav{
  display:flex; gap:1.2rem; font-family:var(--pico-font-family-monospace);
  font-size:.86rem;
}
.site-nav a{
  color:var(--muted); text-decoration:none; padding-block:.2rem;
  border-bottom:1px solid transparent; transition:color .15s,border-color .15s;
}
.site-nav a:hover{ color:var(--accent); border-bottom-color:var(--accent); }
.page-head{ margin-top:2.4rem; margin-bottom:1.4rem; }
.page-head h1{
  font-family:var(--pico-font-family-monospace); font-weight:700;
  font-size:clamp(1.6rem,3vw,2.3rem); letter-spacing:-.01em; margin-bottom:.2rem;
}
.page-head .subtitle{ color:var(--muted); margin:0; }
h2,h3{ font-family:var(--pico-font-family-monospace); letter-spacing:-.01em; }
.panel{
  background:var(--panel); border:1px solid var(--border); border-radius:14px;
  padding:1.1rem 1.3rem 1.3rem; margin-block:1.3rem;
}
.panel>:first-child{ margin-top:0; }
.section-title{
  display:flex; align-items:center; gap:.55rem; margin-bottom:.9rem;
  padding-bottom:.6rem; border-bottom:1px solid var(--border);
}
table{ margin:0; }
thead th{
  font-family:var(--pico-font-family-monospace); font-size:.74rem;
  text-transform:uppercase; letter-spacing:.08em; color:var(--muted);
}
tbody tr{ transition:background .12s; }
tbody tr:hover{ background:var(--accent-soft); }
.votes{ font-family:var(--pico-font-family-monospace); color:var(--accent); font-weight:600; }
code,kbd{
  font-family:var(--pico-font-family-monospace); background:var(--accent-soft);
  color:var(--ink); border-radius:6px; padding:.05rem .35rem;
}
.empty{ color:var(--muted); font-style:italic; }
ul.links{ list-style:none; padding:0; }
ul.links li{ padding:.35rem 0; border-bottom:1px solid var(--border); }
ul.links li:last-child{ border-bottom:0; }
dl.help dt{ font-family:var(--pico-font-family-monospace); color:var(--accent); margin-top:.85rem; }
dl.help dd{ color:var(--ink); margin-left:0; padding-left:1rem; border-left:1px solid var(--border); }
.site-foot{
  margin-block:3.5rem 2rem; padding-top:1.1rem;
  border-top:1px solid var(--border); color:var(--muted);
}
")

(defmacro with-bot-page ((&key title subtitle) &body body)
  "Render a complete, styled HTML page sharing the bot's dark terminal theme.
TITLE supplies both the document <title> and the page heading.  SUBTITLE,
when given, is rendered as a lead paragraph.  BODY is spinneret markup for
the page content (typically one or more (:section :class \"panel\" ...))."
  (let ((ctx (gensym "PAGE-CTX"))
        (csuffix (gensym "CSUFFIX")))
    `(let* ((,ctx (make-instance 'bot-context
                                 :bot-web-port (acceptor-port (request-acceptor *request*))))
            (,csuffix (let ((c (and (boundp 'hunchentoot:*request*) (get-parameter "c"))))
                        (if (and c (plusp (length c)))
                            (format nil "?c=~A" (url-encode c))
                            ""))))
       (spinneret:with-html-string
         (:doctype)
         (:html :lang "en" :data-theme "dark"
          (:head
           (:meta :charset "utf-8")
           (:meta :name "viewport" :content "width=device-width, initial-scale=1")
           (:link :rel "preconnect" :href "https://fonts.googleapis.com")
           (:link :rel "preconnect" :href "https://fonts.gstatic.com" :crossorigin "")
           (:link :rel "stylesheet"
                  :href "https://fonts.googleapis.com/css2?family=IBM+Plex+Sans:wght@400;500;600&family=JetBrains+Mono:wght@500;700&display=swap")
           (:link :rel "stylesheet"
                  :href "https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.min.css")
           (:style (:raw +bot-page-css+))
           (:title ,title))
          (:body
           (:header :class "site-head"
             (:div :class "container"
               (:a :class "brand" :href (make-short-url-string ,ctx "")
                   (bot-nick ,ctx) (:span :class "cursor" "_"))
               (:nav :class "site-nav"
                 (:a :href (make-short-url-string ,ctx (format nil "board~A" ,csuffix)) "board")
                 (:a :href (make-short-url-string ,ctx (format nil "phrases~A" ,csuffix)) "phrases")
                 (:a :href (make-short-url-string ,ctx "") "links")
                 (:a :href (make-short-url-string ,ctx "help") "help"))))
           (:main :class "container"
             (:hgroup :class "page-head"
               (:h1 ,title)
               ,@(when subtitle `((:p :class "subtitle" ,subtitle))))
             ,@body)
           (:footer :class "container site-foot"
             (:small "served by " (bot-nick ,ctx) (:span :class "cursor" " _")))))))))
