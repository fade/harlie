;;;; plugins.lisp

(in-package #:harlie)

(defvar *plugins* (make-hash-table :test 'equal))

(defclass plugin-request ()
  ((plugin-context :initarg :plugin-context :accessor plugin-context)
   (plugin-cmd :initarg :botcmd :accessor plugin-cmd)
   (plugin-conn :initarg :connection :accessor plugin-conn)
   (plugin-channel-name :initarg :channel-name :accessor plugin-channel-name)
   (plugin-reply-to :initarg :reply-to :accessor plugin-reply-to)
   (plugin-token-text-list :initarg :token-text-list :accessor plugin-token-text-list)
   (plugin-action :initarg :action :initform :run :accessor plugin-action)))

(defclass plugin ()
  ((plugin-name :initarg :plugin-name :accessor plugin-name)
   (plugin-hook :initarg :plugin-hook :accessor plugin-hook)
   (plugin-doc :initform nil :accessor plugin-doc)))

;; *doublehelp* is the fallback plugin which gives documentation on a failed lookup.

(defparameter *doublehelp*
  (make-instance 'plugin
		 :plugin-name "DOUBLEHELP"
		 :plugin-hook #'(lambda (plug-request)
				  (cond ((eq (plugin-action plug-request) :docstring)
					 (list (format nil "Sorry, I don't recognize that command.")
					       (format nil "  Try ~A for a list of commands." (make-short-url-string (plugin-context plug-request) "help"))))
					(t nil)))))

(defmacro defplugin (funame args &rest body)
  `(setf (gethash (symbol-name (quote ,funame)) *plugins*)
	 (make-instance 'plugin :plugin-name (symbol-name (quote ,funame))
				:plugin-hook #'(lambda (,@args) ,@body))))

;; So here's the new drill:
;; plugins are expected to respond to the protocol exemplified in sources.
;; If (plugin-action plug-request) is :docstring, then return a documentation string.
;; If (plugin-action plug-request) is :priority, then return a float representing
;; the position of this plugin among all the plugins.
;;
;; In the help page, plugins are listed in numeric order by priority,
;; and by name within priority.
;;
;; Plugins with priorities <= 0.0 are not listed on the help page,
;; but you can still get online help for them with the !help command.
;;
;; On the help page, there's a small vertical break between groups of
;; plugins at integer intervals.  Hence, the plugins with priorities
;; in the range [1.0, 2.0) will be listed in one block, and those in
;; [2.0, 3.0) will be listed in another, etc.  This lets you group
;; plugins together by general type and micromanage their order within
;; a group if you wish.

(defplugin sources (plug-request)
  (case (plugin-action plug-request)
    (:docstring (format nil "Tell us where the source is kept."))
    (:priority -1.0)
    (:run (format nil "git@coruscant.deepsky.com:harlie.git"))))

(defplugin status (plug-request)
  (case (plugin-action plug-request)
    (:docstring (format nil "Give the count of phrases in the chaining database"))
    (:priority 1.0)
    (:run (append (if (and (> (length (plugin-token-text-list plug-request)) 1)
			   (string-equal (second (plugin-token-text-list plug-request)) "full" ))
		      (list (format nil "~A running ~A ~A on ~A ~A, ~A ~A.~%"
				    (machine-instance) (lisp-implementation-type) (lisp-implementation-version)
				    (software-type) (software-version) (machine-type) (machine-version)))
		      nil)
		  (list (format nil "I know ~:D phrases." (count-phrases (plugin-context plug-request))))))))

(defplugin conv (plug-request)
  (case (plugin-action plug-request)
    (:docstring (format nil "Convert between currencies.  Usage: !conv <amount> <from-abbrev> <to-abbrev>"))
    (:priority 2.0)
    (:run (let* ((amount (second (plugin-token-text-list plug-request)))
		 (from (string-upcase (third (plugin-token-text-list plug-request))))
		 (to (string-upcase (fourth (plugin-token-text-list plug-request)))))
	    (if (string= from to)
		(format nil "Converting ~A to ~A is redundant." from to)
		(let* ((data (find-forex
			      (fetch-formatted-url
			       "http://www.xe.com/ucc/convert/?Amount=~A&From=~A&To=~A"
			       amount from to)))
		       (oline (list (first data) from (second data) to)))
		  (format nil "~{~A ~A~^ = ~}" oline)))))))

(defun parse-stock (tick)
  (cond ((string= tick "N/A") nil)
	((float-as-string-p tick) (parse-number tick))
	((or (every #'alpha-char-p tick) (some #'alphanumericp tick)) tick)
	(t nil)))

(defplugin stock (plug-request)
  (case (plugin-action plug-request)
    (:docstring (format nil "Get a vaguely timely appraisal of the
    trading value of a given stock, by symbol. Usage: !stock
    <symbol> (ex: !stock goog)"))
    (:priority 2.0)
    (:run (let* ((symbol (string-upcase (second (plugin-token-text-list plug-request))))
		 (quote (loop for i in (get-stock-values symbol)
			      :collect (parse-stock i))))
	    (if (every #'identity quote)
		;; if you're reading this and you're new to lisp, this
		;; is possibly confusing, but lisp has namespaces for
		;; functions as well as symbols, and many other
		;; things, which allows them to share names that are
		;; disambiguated by context.
		(format nil "Issue: ~A last traded for $~$ at ~A on ~A, ~A changed on the day. Opened at $~$ with a high of $~$ and a low of $~$. ~:D shares traded."
			(first quote) (second quote) (fourth quote) (third quote)
			(fifth quote) (sixth quote) (seventh quote) (eighth quote) (ninth quote))
		(format nil "No quotes for symbole: ~A. Perhaps you mistyped?" symbol))))))

;; (defplugin jcw (plug-request)
;;   (case (plugin-action plug-request)
;;     (:docstring (format nil "Address the left/right libertarian/asshole continuum"))
;;     (:priority 4.0)
;;     (:run (format nil "FUCK YOU, JACKHOLE!"))))

;; (defplugin rally (plug-request)
;;   (case (plugin-action plug-request)
;;     (:docstring (format nil "Deal with the world of racing"))
;;     (:priority 4.0)
;;     (:run (format nil "FUCK YOU, HANS!"))))

(defplugin f1 (plug-request)
  (case (plugin-action plug-request)
    (:docstring (format nil "He's such a fucking nancyboy."))
    (:priority 4.0)
    (:run (format nil "FUCK YOU, HAMILTON!"))))

(defplugin spew (plug-request)
  (case (plugin-action plug-request)
    (:docstring (format nil "What's the secret word for tonight?"))
    (:priority -1.0)
    (:run (list "I'm a mouthy bastard" "Who can't get everything" "He wants to say" "In one line."))))

(defplugin ccode (plug-request)
  (case (plugin-action plug-request)
    (:docstring (format nil "Look up a country code based on a search string.  Usage: !ccode <substring>"))
    (:priority 3.0)
    (:run (let* ((country (second (plugin-token-text-list plug-request)))
		 (countries (country-lookup country)))
	    (if (and countries (listp countries))
		(loop for (a . b) in countries
		      :collect (format nil "[ ~a ][ ~a ]" a b))
		(format nil "No match for search term: ~A" country))))))

(defplugin area (plug-request)
  (case (plugin-action plug-request)
    (:docstring (format nil "Look up an area code.  Usage: !area <area code>"))
    (:priority 3.0)
    (:run (let* ((searchterm (second (plugin-token-text-list plug-request)))
		 (area (areacode-lookup searchterm)))
	    (if (and area (listp area))
		(loop for (a . b) in area
		      :collect (format nil "[ ~A ][ ~A ]" a b))
		(format nil "No area code found for your search term: ~A" searchterm))))))

(defplugin iata (plug-request)
  (case (plugin-action plug-request)
    (:docstring (format nil "Look up an airport description by its IATA code.  Usage: !iata <airport code>"))
    (:priority 3.0)
    (:run (let* ((searchterm (second (plugin-token-text-list plug-request)))
		 (airports (airport-lookup searchterm)))
	    (if (and airports (listp airports))
		(loop for (a . b) in airports
		      :collect (format nil "[ ~A ][ ~A ]" a b))
		(format nil "No match for your airport: ~A" searchterm))))))

(defplugin ciso (plug-request)
  (case (plugin-action plug-request)
    (:docstring (format nil "Look up an ISO currency code.  Usage: !ciso <substring>"))
    (:priority 2.0)
    (:run (let* ((searchterm (second (plugin-token-text-list plug-request)))
		 (curr (currency-lookup searchterm)))
	    (if (and curr (listp curr))
		(loop for (a . b) in curr
		      :collect (format nil "[ ~A ][ ~A ]" a b))
		(format nil "~A" curr))))))


(defplugin rate (plug-request)
  (case (plugin-action plug-request)
    (:docstring (format nil "Look up the conversion rate between two currencies.  Usage: !rate <curr1> <curr2>"))
    (:priority 2.0)
    (:run (let* ((amount 1)
		 (from (string-upcase (second (plugin-token-text-list plug-request))))
		 (to (string-upcase (third (plugin-token-text-list plug-request)))))
	    (if (string= from to)
		(format nil "The rate of ~A in ~A is obvious." from to)
		(progn
		  (let* ((fx
			   (break-on-no-break-space
			    (find-forex (fetch-formatted-url
					 "http://www.xe.com/ucc/convert/?Amount=~A&From=~A&To=~A"
					 amount from to))))
			 (c1amt (parse-number:parse-number
				 (remove #\, (first (first fx)))))
			 (c2amt (parse-number:parse-number
				 (remove #\, (first (second fx)))))
			 (c1->c2 (format nil " ~$ ~A  =  ~$ ~A "
					 c1amt from c2amt to))
			 (c2->c1 (format nil " ~$ ~A  =  ~$ ~A "
					 amount to (/ c1amt c2amt ) from)))
		    (format nil "[ ~A | ~A ]" c1->c2 c2->c1))))))))

(defplugin babble (plug-request)
  (case (plugin-action plug-request)
    (:docstring (format nil "Generate some ill-considered bot spew."))
    (:priority -1.0)
    (:run (format nil "~{~A~^ ~}" (chain (plugin-context plug-request))))))

(defplugin haiku (plug-request)
  (case (plugin-action plug-request)
    (:docstring (format nil "each utterance a / closed flower, capable of / independent life."))
    (:priority 1.5)
    (:run (progn
	    (format nil "~{~A~^ ~}" (make-haiku (plugin-context plug-request)))))))

(defplugin ftw (plug-request)
  (case (plugin-action plug-request)
    (:docstring (format nil "How do you like your victory?"))
    (:priority -1.0)
    (:run (format nil "VICTORY!  FLAWLESS!"))))

(defplugin trigger (plug-request)
  (case (plugin-action plug-request)
    (:docstring (format nil "Return the current trigger word list"))
    (:priority 1.5)
    (:run (format nil "~{~A~^, ~}"
		  (trigger-list (gethash (plugin-channel-name plug-request)
					(channels (plugin-conn plug-request))))))))

(defplugin help (plug-request)
  (case (plugin-action plug-request)
    (:docstring (format nil "Advertise how to get help with the bot's commands"))
    (:priority 1.0)
    (:run (if (> (length (plugin-token-text-list plug-request)) 1)
	      (funcall (plugin-hook
			(gethash (string-upcase (remove #\! (second (plugin-token-text-list plug-request)))) *plugins* *doublehelp*))
		       (make-instance 'plugin-request :action :docstring))
	      (list
	       (format nil "~A" (make-short-url-string (plugin-context plug-request) "help"))
	       (format nil "  or !help <command>"))))))

(defun html-help ()
  "Return HTML for a page giving help with the bot's plugin commands."
  (with-html-output-to-string (s)
    (:html
     (:head
      (:title "Bot Help Page"))
     (:body
      (:h1 "Bot Command Help")
      (:dl
       (let ((oldpriority 0.0))
	 (dolist (doc (sort (plugin-docs) 'sort-docs))
	   (multiple-value-bind (n f) (floor (third doc))
	     (declare (ignore n))
	     (format t "~A ~A~%" oldpriority (third doc))
	     (if (> (third doc) 0.0)
		 (progn
		   (if (and (< oldpriority (third doc)) (= f 0.0))
		       (htm
			(:br)))
		   (htm
		    (:dt (:b (str (format nil "!~A:" (first doc)))))
		    (:dd (str (escape-string (format nil "~A" (second doc))))))
		 (setf oldpriority (third doc))))))))))))

(defplugin 8ball (plug-request)
  (case (plugin-action plug-request)
    (:docstring (format nil "Consult the Magic 8-Ball if you dare"))
    (:priority 4.0)
    (:run (format nil "~A" (consult-8ball)))))

(defplugin calc (plug-request)
  (case (plugin-action plug-request)
    (:docstring (format nil "Unit conversion and other exciting things."))
    (:priority 2.0)
    (:run (let* ((search-tokens (mapcar 'url-encode (cdr (plugin-token-text-list plug-request))))
		 (calcresult (find-calc (fetch-formatted-url "http://www.google.com/search?q=~{~A~^+~}&client=ubuntu&channel=fs" search-tokens))))
	    calcresult))))

(defplugin pants (plug-request)
  (case (plugin-action plug-request)
    (:docstring (format nil "Make the dicklicking face again"))
    (:priority 4.0)
    (:run (format nil "~{~A~^ ~}" (loop for i from 1 to (+ 3 (random 8)) collecting (random-elt *bong-noises*))))))

(defplugin doomsday (plug-request)
  (case (plugin-action plug-request)
    (:docstring (format nil "Check the purity of essence of your precious bodily fluids"))
    (:priority 4.0)
    (:run (find-doomsday (fetch-formatted-url "http://www.thebulletin.org/")))))

(define-condition unrecognized-operator-error (error)
  ((unrecognized-operator :initarg :unrecognized-operator :initform nil :reader unrecognized-operator)))

(defparameter *unitconv* (acons '("CELSIUS" "FAHRENHEIT") '("9" "5" "/" "*" "32" "+") nil))

(defun rpn-calculator (l)
  (let ((stack nil))
    (handler-case
	(do* ((x (pop l) (pop l))
	      (opname (string-upcase x) (string-upcase x)))
	     ((not x) 	  (if (= (length stack) 1)
			      (if (integerp (caar stack))
				  (format nil "~A" (caar stack))
				  (format nil "~F" (caar stack)))
			      (format nil "Stack (top): [~{~A~^ ~}]" (maplist #'(lambda (x) (car x)) stack))))
		    (let ((opname (string-upcase x)))
		      (cond ((scan "^[0-9]+$" x)
			     (push (list (parse-integer x) nil) stack))
			    ((scan "^[0-9]*[.][0-9]+$" x)
			     (push (list (parse-number:parse-number x) nil) stack))
			    ((scan "^NEG$" opname)
			     (setf (caar stack) (- (caar stack))))
			    ; Commutative binary operators
			    ((scan "^[+*]$" opname)
			     (push (list (funcall (intern opname) (car (pop stack)) (car (pop stack))) nil) stack))
			    ; Non-commutative binary operators
			    ((scan "^[/-]|EXPT|LOG|MOD$" opname)
			     (let ((a (car (pop stack)))
				   (b (car (pop stack))))
			       (push (list (funcall (intern opname) b a) nil) stack)))
			    ; Forth-style stack modifying operators
			    ((scan "^SWAP$" opname)
			     (setf stack (append (reverse (subseq stack 0 2)) (cddr stack))))
			    ((scan "^DROP$" opname)
			     (pop stack))
			    ((scan "^DUP$" opname)
			     (push (car stack) stack))
			    ; Unary operators whose names match those in Common Lisp
			    ((scan "^SIN|COS|TAN|ASIN|ACOS|ATAN|SQRT|EXP$" opname)
			     (push (list (funcall (intern opname) (car (pop stack))) nil) stack))
			    ; Binary operators expecting and requiring integer arguments
			    ((scan "^GCD|LCM$" opname)
			     (handler-case
				 (push (list (funcall (intern opname) (car (pop stack)) (car (pop stack))) nil) stack)
			       (type-error () (return-from rpn-calculator "Type error: expected integer."))))
			    ((scan "^RAND(OM)?$" opname)
			     (push (list (random (car (pop stack))) nil) stack))
			    ((scan "^LN$" opname)
			     (push (list (log (pop stack)) nil) stack))
			    (t (error 'unrecognized-operator-error
				      :unrecognized-operator x)))))

      (simple-type-error () (if (= (length stack) 0)
				 (format nil "Stack underflow.")
				 (format nil "Type error.")))
      (unrecognized-operator-error (x) (format nil "Unrecognized operator: ~A" (unrecognized-operator x))))))

(defplugin rpn (plug-request)
  (case (plugin-action plug-request)
    (:docstring (format nil "Do a little arithmetic in reverse Polish notation"))
    (:priority 3.0)
    (:run (rpn-calculator (rest (plugin-token-text-list plug-request))))))

(defparameter *metar-datums* (make-hash-table :test 'equal :size 10000))

(defparameter *metar-last-scrape* nil)

(defun clear-the-decks ()
  (setf *metar-datums* (make-hash-table :test 'equal :size 10000))
  (setf *metar-last-scrape* nil))

(defun tronna ()
  (gethash "CYYZ" *metar-datums*))

(defparameter *metar-regexen*
  ; Sample METAR: "CYVR 211900Z 28008KT 30SM FEW040 FEW170 BKN240 11/06 A3019 RMK CF1AC1CI2 SLP224"
  '("\\s([0-9]+[Z])\\s" ; " 211900Z " (DDHHMMZ: day of month, hour, minute, in time Zulu)
    "\\s(M?[0-9]+)[/](M?[0-9]+)\\s" ; " 11/06 " (temperature and dewpoint)
    "\\s[0-9]{3}([0-9]+)(G[0-9]+)?([A-Z]+)\\s" ; " 28008KT " (DDDWW: direction, wind speed.  Optional G[0-9]+ for gusts.)
;    "^([A-Z]{4})\\s" ; "CYVR " (ICAO station code)
    ))

(defun valid-metar (s)
  (if (and (every #'(lambda (rx) (scan rx s)) *metar-regexen*)
	   (every #'upper-case-p (subseq s 0 4))
	   (every #'digit-char-p (subseq s 5 11)))
      (values (subseq s 0 4)
	       (parse-number (subseq s 7 9))
	       (parse-number (subseq s 9 11)))
      nil))

(defun scrape-metar-data (&optional (hoursback 0))
  (let ((flexi-streams:*substitution-char* #\?)
	(base-timestamp (timestamp-minimize-part
			 (adjust-timestamp (now) (offset :hour (- hoursback)))
			 :hour :timezone +utc-zone+)))
    (with-open-stream
	(metar-stream (http-request
		       (format nil "http://weather.noaa.gov/pub/data/observations/metar/cycles/~2,'0dZ.TXT"
			       (mod (- (current-zulu-hour) hoursback) 24))
		       :want-stream t))
      (mapc #'(lambda (l)
		(multiple-value-bind (station hours minutes) (valid-metar l)
		  (when (and station hours minutes)
		    (let ((ts (timestamp+ (timestamp+ base-timestamp hours :hour) minutes :minute))
			  (oldts (gethash station *metar-datums* nil)))
		      (when (or (not oldts) (timestamp< (car oldts) ts))
			(setf (gethash station *metar-datums*)
			      (list ts l)))))))
	    (do* ((l (read-line metar-stream nil 'eof) (read-line metar-stream nil 'eof))
		  (metars (make-hash-table :test 'equal :size 10000) metars ))
		 ((eq l 'eof) (hash-table-keys metars))
	      (when (and (stringp l) (> (length l) 4) (upper-case-p (aref l 0))) (setf (gethash l metars) t))))))
  nil)

(defun check-metar-data ()
  (with-connection (psql-botdb-credentials *bot-config*)
    (loop for k being the hash-keys in *metar-datums*
	  when (not (query (:select 'icao 'iata 'airport 'location
				    :from 'icao-code
				    :where (:= 'icao k))))
	    collecting k)))

(defun retrospective-metar-scrape (&optional (hoursback 0))
    (unless (and (= hoursback 0)
		 *metar-last-scrape*
		 (< (timestamp-diff *metar-last-scrape* (now)) 1200))
      (scrape-metar-data hoursback)
      (setf *metar-last-scrape* (now))))

(defun metar-temp-value (centigrade &optional (units :Centigrade))
  (cond ((null centigrade) nil)
	((eq units :Fahrenheit) (format nil "~,1F F" (+ 32 (* (/ 9 5) centigrade))))
	((eq units :Kelvin) (format nil "~,1F K" (+ 273.15 centigrade)))
	(t (format nil "~,1F C" centigrade))))

(defun metar-temp-to-c (s)
  "Convert a temperature measure to degrees Centigrade."
  (let ((centigrade (if (scan "^M" s)
			(- (parse-integer (subseq s 1)))
			(parse-integer s))))
    centigrade))

(defun metar-windspeed-to-kmh (windspeed units)
  "Convert a given windspeed value to km/h."
  (cond ((string= units "KT") (* 1.852 windspeed))
	((string= units "MPS") (* 3.6 windspeed))
	(t nil)))

(defun current-zulu-hour ()
  "Return the current hour in Zulu time."
  (multiple-value-bind (ns sec min hour) (decode-timestamp (now) :timezone +utc-zone+)
    (declare (ignore ns sec min))
    hour))

(defun read-metar-data (regex zulu)
  "Fetch one METAR data file, search it for a matching line, and return it if found; return nil otherwise."
  (with-open-stream
      (metar-stream (http-request
		     (format nil "http://weather.noaa.gov/pub/data/observations/metar/cycles/~2,'0dZ.TXT" zulu)
		     :want-stream t))
    (do* ((l (read-line metar-stream nil 'eof) (read-line metar-stream nil 'eof))
	  (payload nil))
	 ((or (eq l 'eof) payload) payload)
      (when (scan regex l) (setf payload l)))))

(defun metar-lookup-by-icao (icao &optional (units :Centigrade))
  (clear-the-decks)
  (scrape-metar-data 1)
  (scrape-metar-data 0)
  (let ((the-metar (gethash icao *metar-datums*)))
    (if the-metar
	(let ((metar-line (second the-metar)))
	  (multiple-value-bind (station-name time-string windspeed cur-temp dew-temp) (metar-extract-data metar-line)
	    (let ((windchill (calculate-wind-chill cur-temp windspeed))
		  (humidex (calculate-humidex cur-temp dew-temp)))
	      (apply #'format nil "~A ~A   Current temperature ~A~@[, wind chill ~A~]~@[, humidex ~A~], dewpoint ~A"
		      station-name time-string
		      (mapcar #'(lambda (x) (metar-temp-value x units)) (list cur-temp windchill humidex dew-temp))))))
	nil)))

(defun metar-lookup (term &optional (units :Centigrade))
  (with-connection (psql-botdb-credentials *bot-config*)
    (if (query (:select 'icao :from 'icao-code :where (:= 'icao (string-upcase term))))
	(list (metar-lookup-by-icao (string-upcase term) units)) 
	(let ((icaos (query (:select 'icao :from 'icao-code :where (:= 'iata term)))))
	  (if icaos
	      (mapcar #'(lambda (x) (metar-lookup-by-icao (car x) units)) icaos)
	      (let ((icaos (query (:select 'icao :from 'icao-code :where
					   (:or (:ilike 'airport (format nil "%~A%" term))
						(:ilike 'location (format nil "%~A%" term)))))))
		(if icaos
		    (mapcar #'(lambda (x) (metar-lookup-by-icao (car x) units)) icaos)
		    (format nil "Sorry, I don't know from ~A." term))))))))

(defun obtain-metar-data (regex &optional (units :Centigrade))
  "Grovel through up to 6 METAR data files to find a matching line, and return its meteorological description."
  (let* ((flexi-streams:*substitution-char* #\?)
	 (zulu-hour (current-zulu-hour))
	 (metar-line (loop for zulu from zulu-hour downto (- zulu-hour 5)
			   thereis (read-metar-data regex zulu))))
    (if metar-line
	(multiple-value-bind (station-name time-string windspeed cur-temp dew-temp) (metar-extract-data metar-line)
	  (let ((windchill (calculate-wind-chill cur-temp windspeed))
		(humidex (calculate-humidex cur-temp dew-temp)))
	      (format nil "~A ~A   Current temperature ~A~@[, wind chill ~A~]~@[, humidex ~A~], dewpoint ~A"
		      station-name time-string
		      (metar-temp-value cur-temp units) (metar-temp-value windchill units)
		      (metar-temp-value humidex units) (metar-temp-value dew-temp units))))
	"Temperature data not available.")))

(defun metar-extract-data (metar-line)
  "Extract the values from various fields in a METAR record."
  (multiple-value-bind (tempnonce temp-substrings)
      (scan-to-strings "^([^\\s]*)\\s*([0-9]+[Z]).*[0-9][0-9][0-9]([0-9]+)(G[0-9]+)?([A-Z]+).*\\s(M?[0-9]+)[/](M?[0-9]+)\\s" metar-line)
    (declare (ignore tempnonce))
    (if (not (null temp-substrings))
	(values (aref temp-substrings 0) (aref temp-substrings 1)
		(metar-windspeed-to-kmh (parse-integer (aref temp-substrings 2)) (aref temp-substrings 4)) 
		(metar-temp-to-c (aref temp-substrings 5)) 
		(metar-temp-to-c (aref temp-substrings 6)))
	nil)))

(defun calculate-wind-chill (ambient windspeed)
  (if (or (not windspeed) (> ambient 10 ) (< windspeed 4.8))
      nil
      (let* ((windpow (expt windspeed 0.16))
	     (windchill (+ 13.12 (* 0.6215 ambient) (* -11.37 windpow) (* 0.3965 ambient windpow))))
	windchill)))

(defun calculate-humidex (ambient dewpoint)
  (let* ((kdewpoint (+ 273.16 dewpoint))
	 (e (* 6.11 (exp (* 5417.7530 (- (/ 1 273.16) (/ 1 kdewpoint))))))
	 (h (* 0.5555 (- e 10.0)))
	 (humidex (+ ambient h)))
    (if (and (>= ambient 20) (> dewpoint 0) (>= humidex 25))
	humidex
	nil)))

(defun metar-units-symbol (s)
  "Return the temperature-scale-name symbol corresponding to the specified string."
  (cond ((scan "^[kK]" s) :Kelvin)
	((scan "^[fFiI]" s) :Fahrenheit)
	(t :Centigrade)))

; http://www.avcodes.co.uk/aptcodesearch.asp

(defplugin oldmetar (plug-request)
  (case (plugin-action plug-request)
    (:docstring (format nil "Print a human-readable weather report based on METAR data.  Usage: !oldmetar <ICAO>"))
    (:priority -1.0)
    (:run
     (format t "~{~^~A ~}~%" (plugin-token-text-list plug-request))
     (let* ((tokens (plugin-token-text-list plug-request))
	    (location (if (<= 2 (length tokens))
			  (string-upcase (second tokens))
			  "CYYZ"))
	    (units (if (<= 3 (length tokens))
		       (metar-units-symbol (third tokens))
		       :Centigrade))
	    (the-metar (metar-lookup-by-icao location units)))
	 (if the-metar
	     the-metar
	     (format nil "Sorry, I don't know from ~A." location))))))

(defplugin metar (plug-request)
  (case (plugin-action plug-request)
    (:docstring (format nil "Print a human-readable weather report based on METAR data.  Usage: !metar <ICAO>"))
    (:priority 2.0)
    (:run
     (let* ((tokens (plugin-token-text-list plug-request))
	    (location (if (<= 2 (length tokens))
			  (string-upcase (second tokens))
			  "CYYZ"))
	    (units (if (<= 3 (length tokens))
		       (metar-units-symbol (third tokens))
		       :Centigrade))
	    (metar-tree (http-request (format nil "http://aviationweather.gov/adds/metars/?station_ids=~A&std_trans=standard&chk_metars=on&hoursStr=most+recent+only&submitmet=Submit" location)))
	    (metar-line (find-metar metar-tree)))
       (if metar-line
	   (multiple-value-bind (station-name time-string windspeed cur-temp dew-temp) (metar-extract-data metar-line)
	     (let ((windchill (calculate-wind-chill cur-temp windspeed))
		   (humidex (calculate-humidex cur-temp dew-temp)))
	       (apply #'format nil "~A ~A   Current temperature ~A~@[, wind chill ~A~]~@[, humidex ~A~], dewpoint ~A"
		      station-name time-string
		      (mapcar #'(lambda (x) (metar-temp-value x units)) (list cur-temp windchill humidex dew-temp)))))
	   (format nil "Sorry, I don't know from ~A." location)))     )))

(defplugin weather (plug-request)
  (case (plugin-action plug-request)
    (:docstring (format nil "Print a human-readable weather report based on METAR data"))
    (:priority -2.0)
    (:run
     (format t "~{~^~A ~}~%" (plugin-token-text-list plug-request))
     (let* ((tokens (plugin-token-text-list plug-request))
	    (location (if (<= 2 (length tokens))
			  (string-upcase (second tokens))
			  "CYYZ"))
	    (units (if (<= 3 (length tokens))
		       (metar-units-symbol (third tokens))
		       :Centigrade)))
       (remove-if-not #'identity (metar-lookup location units))))))

;; temperature conversions

(defun temperature-convert (temp &key (scale :celsius))
  (let ((tt (ignore-errors (parse-number temp))))
    ;; (assert (numberp tt))
    (when (numberp tt)
      (case scale
	(:celsius (+ (* tt 9/5) 32)) 
	(:fahrenheit (* (- tt 32) 5/9))
	))))

;; ftoc fahrenheit -> celsius

(defplugin ftoc (plug-request)
  (case (plugin-action plug-request)
    (:docstring (format nil "Convert a value in fahrenheit to celsius."))
    (:priority 3.0)
    (:run (let* ((fahrenheit (second (plugin-token-text-list plug-request)))
		 (celsius (temperature-convert fahrenheit :scale :fahrenheit)))
	    (if celsius
		(format nil "~f F == ~0,3f C" fahrenheit celsius)
		(format nil "Error in converting unit: ~A" fahrenheit))))))

;; ctof celsius -> fahrenheit

(defplugin ctof (plug-request)
  (case (plugin-action plug-request)
    (:docstring (format nil "Convert a value in celsius to fahrenheit."))
    (:priority 3.0)
    (:run (let* ((celsius (second (plugin-token-text-list plug-request)))
		 (fahrenheit (temperature-convert celsius)))
	    (if fahrenheit
		(format nil "~f C == ~0,3f F" celsius (temperature-convert celsius))
		(format nil "Error in converting unit: ~A" celsius))))))


;; ===[ hyperspace motivator follows. ]===

(defun plugin-docs ()
  "Generate a list-of-lists from the plugin names, doc strings, and priorities."
  (mapcar #'(lambda (k) (list k
			      (funcall (plugin-hook (gethash k *plugins*))
				       (make-instance 'plugin-request :action :docstring))
			      (funcall (plugin-hook (gethash k *plugins*))
				       (make-instance 'plugin-request :action :priority))))
	  (hash-table-keys *plugins*)))

(defun sort-docs (a b)
  "Sort a list-of-lists of plugins by name within priority."
  (cond ((not (= (third a) (third b))) (< (third a) (third b)))
	((not (string= (first a) (first b))) (string< (first a) (first b)))
	(t nil)))

(defun run-plugin (plug-request)
  "Run a plugin's hook function and send the return text back to the requester."
  (let* ((plugname (string-upcase (subseq (plugin-cmd plug-request) 1)))
	 (plugf (gethash plugname *plugins* nil)))
    (if plugf
	(let ((reply (funcall (plugin-hook plugf) plug-request)))
	  (cond ((stringp reply)
		 (qmess (plugin-conn plug-request) (plugin-reply-to plug-request)
			(format nil "~A:: ~A" (string-downcase plugname) reply)))
		((listp reply)
		 (dolist (line reply)
		   (qmess (plugin-conn plug-request) (plugin-reply-to plug-request)
			  (format nil "~A:: ~A" (string-downcase plugname) line))))
		(t (qmess (plugin-conn plug-request) (plugin-reply-to plug-request)
			  (format nil "~A:: I'm a tragic victim of duck typing gone wrong." (string-downcase plugname))))))
	(qmess (plugin-conn plug-request) (plugin-reply-to plug-request) (format nil "~A: unknown command." (string-downcase plugname))))))
