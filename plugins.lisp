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
    (:run (format nil "https://github.com/fade/harlie.git"))))

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
		 (to (string-upcase (fourth (plugin-token-text-list plug-request))))
                 (pair (str:join "" (list from to))))
            (declare (ignorable pair))
	    (cond ((string= from to)
		   (format nil "Converting ~A to ~A is redundant." from to))
                  (t
                   (handler-case
                       (trivial-timeout:with-timeout (10)
		         (let* ((data (convert-pairs from to amount)))
		           (format nil "~A" (cdr (assoc :text data)))))
                     (trivial-timeout:timeout-error (c)
                       (format nil "Timeout error: ~A~%" c)))))))))

(defplugin rate (plug-request)
  (case (plugin-action plug-request)
    (:docstring (format nil "Look up the conversion rate between two currencies.  Usage: !rate <curr1> <curr2>"))
    (:priority 2.0)
    (:run (let* ((amount 1)
		 (from (string-upcase (second (plugin-token-text-list plug-request))))
		 (to (string-upcase (third (plugin-token-text-list plug-request)))))
            (format t "~2&||  ~A ~A ~A~2%" amount from to)
	    (if (string= from to)
		(format nil "The rate of ~A in ~A is obvious." from to)
		(progn
		  (let* ((fx (convert-pairs from to amount)) ;; <-- alist of vals from remote API.
			 (c1amt amount)
			 (c2amt (cdr (assoc :value fx)))
			 (c1->c2 (format nil " ~$ ~A  =  ~$ ~A "
					 c1amt from c2amt to))
			 (c2->c1 (format nil " ~$ ~A  =  ~$ ~A "
					 amount to (/ c1amt c2amt ) from)))
		    (format nil "[ ~A | ~A ]" c1->c2 c2->c1))))))))

(defplugin stock (plug-request)
  (case (plugin-action plug-request)
    (:docstring (format nil "Get a vaguely timely appraisal of the
    trading value of a given stock, by symbol. Usage: !stock
    <symbol> (ex: !stock goog)"))
    (:priority 2.0)
    (:run (let* ((symbol (string-upcase (second (plugin-token-text-list plug-request))))
		 (quote (make-stock symbol)))
	    (if quote
		(format nil "Issue: ~A opened at $~$ with high of ~A low of ~A, closing at ~A with volume of ~A"
			(stock-name quote)  (stock-open quote) (stock-high quote) (stock-low quote)
                        (stock-close quote) (stock-volume quote))
		(format nil "No quotes for symbol: ~A. Perhaps you mistyped?" symbol))))))

(defplugin toynb (plug-request)
  (case (plugin-action plug-request)
    (:docstring (format nil "Message of unknown origin."))
    (:priority 4.0)
    (:run (list "TOYNBEE IDEA" "IN MOVIE `2001" "RESURRECT DEAD" "ON PLANET JUPITER"))))

(defplugin htp (plug-request)
  (case (plugin-action plug-request)
    (:docstring (format nil "Return a random quote from the movie 'Hackers'. Usage: !htp"))
    (:priority 4.0)
    (:run (list (random-elt *hackers*)))))

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
    (:run (let* ((pokearoo (second (plugin-token-text-list plug-request))) ;; < -- introduced for debuggery. could be collapsed into the next assignment.
                 (searchterm (string-trim "*" pokearoo))
		 (area (areacode-lookup searchterm)))
            ;; (format t "~&~A || ~A~%" pokearoo searchterm)
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
  (let* ((context (make-instance 'bot-context :bot-web-port (acceptor-port (request-acceptor *request*))))
         (bothandle (bot-nick context)))
    (spinneret:with-html-string
      (:html
       (:head
        (:link :rel "stylesheet" :href "https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.min.css")
        (:title (format nil "~A Help Page" bothandle)))
       (:body
        (:h1 (format nil "~A Help Page" bothandle))
        (:dl
         (let ((oldpriority 0.0))
           (dolist (doc (sort (plugin-docs) 'sort-docs))
             (multiple-value-bind (n f) (floor (third doc))
               (declare (ignore n))
               (log:debug "~A ~A~%" oldpriority (third doc))
               (if (> (third doc) 0.0)
                   (progn
                     (when (and (< oldpriority (third doc)) (= f 0.0))
                       (:br))
                     (:dt (:b (format nil "!~A:" (first doc))))
                     (:dd (format nil "~A" (second doc)))
                     (setf oldpriority (third doc)))))))))))))

(defplugin 8ball (plug-request)
  (case (plugin-action plug-request)
    (:docstring (format nil "Consult the Magic 8-Ball if you dare"))
    (:priority 4.0)
    (:run (format nil "~A" (consult-8ball)))))

(defplugin calc (plug-request)
  (case (plugin-action plug-request)
    (:docstring (format nil "Unit conversion and other exciting things."))
    (:priority 2.0)
    (:run "Scraping Web pages sucks.  Count on your fingers instead.")))

(defplugin pants (plug-request)
  (case (plugin-action plug-request)
    (:docstring (format nil "Make the dicklicking face again"))
    (:priority 4.0)
    (:run (format nil "~{~A~^ ~}" (loop for i from 1 to (+ 3 (random 8)) collecting (random-elt *bong-noises*))))))

(defplugin doomsday (plug-request)
  (case (plugin-action plug-request)
    (:docstring (format nil "Check the purity of essence of your precious bodily fluids"))
    (:priority 4.0)
    (:run (find-doomsday (fetch-formatted-url "https://thebulletin.org/doomsday-clock/current-time/")))))

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
      (division-by-zero () (format nil "Bald Monkey! You will not trick me into dividing by zero!"))
      (simple-type-error () (if (= (length stack) 0)
                                (format nil "Stack underflow.")
                                (format nil "Type error.")))
      (unrecognized-operator-error (x) (format nil "Unrecognized operator: ~A" (unrecognized-operator x))))))

(defplugin rpn (plug-request)
  (case (plugin-action plug-request)
    (:docstring (format nil "Do a little arithmetic in reverse Polish notation"))
    (:priority 3.0)
    (:run (rpn-calculator (rest (plugin-token-text-list plug-request))))))

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

(defun scan-to-first-string (regex target-string)
  (multiple-value-bind (tempnonce temp-substrings)
      (scan-to-strings regex target-string)
      (declare (ignore tempnonce))
    (aref temp-substrings 0)))

(defun metar-extract-data (metar-data)
  "Extract the values from various fields in a METAR record."
  (let* ((md metar-data)
         (station-name (gethash "name" md))
	 (reading-time (gethash "reportTime" md))
	 (wind-speed (gethash "wspd" md))
	 (wind-units (scan-to-first-string "[0-9]{2}(MPS|KT)" (gethash "rawOb" md))) 
	 (metar-temp (gethash "temp" md))
	 (metar-dewpoint (gethash "dewp" md))
	 (windspeed-kmh (metar-windspeed-to-kmh wind-speed wind-units))
	 (temperature-c metar-temp)
	 (dewpoint-c (if metar-dewpoint metar-dewpoint)))
    (values station-name reading-time windspeed-kmh temperature-c dewpoint-c)))

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

(defun form-metar-query-string (location)
  (format nil
	  (format nil "~{~A~^&~}"
		  '("https://aviationweather.gov/api/data/metar?ids=\~\A&format=json"
		    "std_trans=standard"
		    "chk_metars=on"
		    "hoursStr=most+recent+only"
		    "submitmet=Submit")) location))

(defun get-metar (site)
  "when the remote site goes down or changes, it's difficult to see in
the plugin code, so this function returns the parsed json or the get
error."
  (let* ((raw-data (com.inuoe.jzon:parse (http-request (form-metar-query-string site)
                                                       :preserve-uri t :redirect 16))))
    raw-data))

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
            ;; the remote site check will return an empty vector if the location string is invalid.
            (raw-data (com.inuoe.jzon:parse (http-request (form-metar-query-string location)
                                                          :preserve-uri t :redirect 16)))
            (metar-data (if (> (length raw-data) 0)
                            (svref raw-data 0))))
       (if metar-data
           (multiple-value-bind (station-name time-string windspeed cur-temp dew-temp) (metar-extract-data metar-data)
             (if (and station-name time-string windspeed cur-temp dew-temp)
                 (let ((windchill (calculate-wind-chill cur-temp windspeed))
                       (humidex (calculate-humidex cur-temp dew-temp)))
                   (apply #'format nil "~A ~A   Current temperature ~A~@[, wind chill ~A~]~@[, humidex ~A~], dewpoint ~A"
                          station-name time-string
                          (mapcar #'(lambda (x) (metar-temp-value x units)) (list cur-temp windchill humidex dew-temp))))
                 (format nil "Sorry, no weather at ~A." location)))
           (format nil "Sorry, I don't know from ~A. Try a valid ICAO code." location))))))

;; temperature conversions

(defun temperature-convert (temp &key (scale :celsius))
  (let ((tt (if (numberp temp)
                temp
                (ignore-errors (parse-number temp)))))
    (case scale
      (:celsius (+ (* tt 9/5) 32))
      (:fahrenheit (* (- tt 32) 5/9)))))

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

(defplugin pope (plug-request)
  (case (plugin-action plug-request)
    (:docstring (format nil "Report the head of the Roman church."))
    (:priority 2.0)
    (:run (extract-from-html (chtml:parse (webget "http://www.vatican.va/phome_en.htm")
					  (chtml:make-lhtml-builder))
			     'papal-anchor
			     'papal-extractor))))

(define-condition too-many-rolls (error)
  ((number-of-rolls :initarg :number-of-rolls
                    :initform nil
                    :reader number-of-rolls))
  (:documentation "TOO-MANY-ROLLS -- condition signalled when the user calls for too many N of P polyhedral dice."))

(defun roll-aux (n p base)
  (let ((random-state (make-random-state t)))
    (handler-bind
        ((too-many-rolls #'(lambda (c)
                             (return-from roll-aux (format nil "You have requested too many dice: ~D"
                                                           (number-of-rolls c)))))
         (type-error #'(lambda (c)
                         (return-from roll-aux (format nil "~S :: You can't roll non-numeric numbers of polyhedra. This is not Mordor, this is IRC." c)))))
      (cond ((< 1001 n) ;; if n is more than 1001
             (error 'too-many-rolls :number-of-rolls n))
            ((not (and (numberp n) (numberp p)))
             (error 'type-error)))
      (multiple-value-bind (total-roll) (loop for dice from 1 to n
                                              for roll = (max (random p random-state) 1)
                                              ;; collecting roll into runs
                                              summing roll into total-roll
                                              finally (return (values total-roll)))
        (format nil "You roll ~A d~A ... the roll is ~:d!" n base total-roll)))))

(defplugin roll (plug-request)
  (case (plugin-action plug-request)
    (:docstring (format nil "Roll N of P polyhedral dice and return the sum."))
    (:priority 3.0)
    (:run (let* ((n (parse-integer (second (plugin-token-text-list plug-request)) :junk-allowed t))
                 (base (parse-integer (third (plugin-token-text-list plug-request)) :junk-allowed t))
                 (p (1+ base)))
            (roll-aux n p base)))))


;;; ============================================================
;;; Memo / !tell system
;;; ============================================================

(defplugin tell (plug-request)
  (case (plugin-action plug-request)
    (:docstring "Leave a message for someone who's not here. Usage: !tell <nick> <message>")
    (:priority 1.0)
    (:run (let* ((tokens (plugin-token-text-list plug-request))
                 (recipient (second tokens))
                 (message-words (cddr tokens))
                 (sender (prefix-nick (parse-prefix (message-prefix (last-message (plugin-conn plug-request)))))))
            (cond
              ((null recipient)
               "Usage: !tell <nick> <message>")
              ((null message-words)
               (format nil "What do you want to tell ~A?" recipient))
              ((string-equal recipient (connection-nick (plugin-conn plug-request)))
               "I appreciate the thought, but I already know everything I need to.")
              ((string-equal recipient sender)
               "Talking to yourself? I won't judge, but I also won't help.")
              (t
               (let ((memo (store-memo sender recipient
                                       (plugin-channel-name plug-request)
                                       (format nil "~{~A~^ ~}" message-words))))
                 (if memo
                     (format nil "Got it, I'll tell ~A when they're around." recipient)
                     "Something went wrong storing that memo."))))))))

(defplugin memos (plug-request)
  (case (plugin-action plug-request)
    (:docstring "Check if you have any pending memos. Usage: !memos")
    (:priority 1.0)
    (:run (let ((sender (prefix-nick (parse-prefix (message-prefix (last-message (plugin-conn plug-request)))))))
            (if (has-pending-memos-p sender)
                (let ((count (pending-memo-count sender)))
                  (format nil "You have ~D pending memo~:P. They'll be delivered shortly."
                          count))
                "You have no pending memos.")))))

;;; ============================================================
;;; New utility plugins
;;; ============================================================

(defun format-duration (seconds)
  "Format a duration in seconds as a human-readable string."
  (let* ((days    (floor seconds 86400))
         (rem     (mod seconds 86400))
         (hours   (floor rem 3600))
         (rem     (mod rem 3600))
         (minutes (floor rem 60)))
    (cond ((> days 0)   (format nil "~Dd ~Dh ~Dm" days hours minutes))
          ((> hours 0)  (format nil "~Dh ~Dm" hours minutes))
          (t            (format nil "~Dm" minutes)))))

(defplugin uptime (plug-request)
  (case (plugin-action plug-request)
    (:docstring "How long has the bot been running? Usage: !uptime")
    (:priority 1.0)
    (:run (if *boot-time*
              (format nil "Up for ~A" (format-duration (- (get-universal-time) *boot-time*)))
              "I don't know when I started. Existential crisis."))))

(defplugin seen (plug-request)
  (case (plugin-action plug-request)
    (:docstring "When was a user last active? Usage: !seen <nick>")
    (:priority 1.0)
    (:run (let ((target (second (plugin-token-text-list plug-request))))
            (if (null target)
                "Usage: !seen <nick>"
                (handler-case
                    (with-connection (db-credentials *bot-config*)
                      (let ((user (first (select-dao 'harlie-user
                                           (:or (:= 'harlie-user-name target)
                                                (:= 'current-handle target))))))
                        (if user
                            (let* ((last (last-seen user))
                                   (now (local-time:now))
                                   (diff (local-time:timestamp-difference now last)))
                              (format nil "~A was last seen ~A ago."
                                      (current-handle user) (format-duration (floor diff))))
                            (format nil "I've never seen ~A." target))))
                  (error (e)
                    (declare (ignore e))
                    (format nil "Database unavailable, can't look up ~A." target))))))))

(defun wiki-summary (term)
  "Fetch the first sentence of a Wikipedia article via the REST API."
  (handler-case
      (trivial-timeout:with-timeout (10)
        (let* ((encoded (substitute #\_ #\Space term))
               (url (format nil "https://en.wikipedia.org/api/rest_v1/page/summary/~A" encoded))
               (raw (drakma:http-request url
                                         :method :get
                                         :additional-headers '(("User-Agent" . "Consort-IRC-Bot/1.0"))
                                         :want-stream nil))
               (data (com.inuoe.jzon:parse raw)))
          (let ((extract (gethash "extract" data))
                (title (gethash "title" data)))
            (if extract
                (let ((first-sentence (first (str:split-omit-nulls #\. extract))))
                  (format nil "~A: ~A." title first-sentence))
                nil))))
    (error () nil)))

(defplugin wiki (plug-request)
  (case (plugin-action plug-request)
    (:docstring "Look up a Wikipedia summary. Usage: !wiki <term>")
    (:priority 2.0)
    (:run (let ((term (format nil "~{~A~^ ~}" (rest (plugin-token-text-list plug-request)))))
            (if (str:empty? term)
                "Usage: !wiki <term>"
                (or (wiki-summary term)
                    (format nil "No Wikipedia article found for '~A'." term)))))))

(defun dict-lookup (word)
  "Fetch the definition of a word from the Free Dictionary API."
  (handler-case
      (trivial-timeout:with-timeout (10)
        (let* ((url (format nil "https://api.dictionaryapi.dev/api/v2/entries/en/~A"
                            (drakma:url-encode word :utf-8)))
               (raw (drakma:http-request url :method :get :want-stream nil))
               (data (com.inuoe.jzon:parse raw)))
          (when (and (vectorp data) (> (length data) 0))
            (let* ((entry (aref data 0))
                   (meanings (gethash "meanings" entry))
                   (first-meaning (when (and meanings (> (length meanings) 0))
                                    (aref meanings 0)))
                   (part-of-speech (when first-meaning
                                     (gethash "partOfSpeech" first-meaning)))
                   (definitions (when first-meaning
                                  (gethash "definitions" first-meaning)))
                   (first-def (when (and definitions (> (length definitions) 0))
                                (gethash "definition" (aref definitions 0)))))
              (when first-def
                (format nil "~A (~A): ~A" word part-of-speech first-def))))))
    (error () nil)))

(defplugin define (plug-request)
  (case (plugin-action plug-request)
    (:docstring "Look up a word definition. Usage: !define <word>")
    (:priority 2.0)
    (:run (let ((word (second (plugin-token-text-list plug-request))))
            (if (null word)
                "Usage: !define <word>"
                (or (dict-lookup word)
                    (format nil "No definition found for '~A'." word)))))))

(defvar *timezones-loaded* nil)

(defun ensure-timezones-loaded ()
  "Make sure the full timezone repository is loaded."
  (unless *timezones-loaded*
    (local-time:reread-timezone-repository)
    (setf *timezones-loaded* t)))

(defplugin tz (plug-request)
  (case (plugin-action plug-request)
    (:docstring "Show current time in a timezone. Usage: !tz <timezone> (e.g. US/Eastern, Europe/London)")
    (:priority 3.0)
    (:run (let ((zone-name (second (plugin-token-text-list plug-request))))
            (if (null zone-name)
                "Usage: !tz <timezone> (e.g. US/Eastern, Europe/London, Asia/Tokyo)"
                (handler-case
                    (progn
                      (ensure-timezones-loaded)
                      (let ((tz (local-time:find-timezone-by-location-name zone-name)))
                        (if tz
                            (let* ((now (local-time:now))
                                   (formatted (local-time:format-timestring
                                               nil now
                                               :format '((:year 4) #\- (:month 2) #\- (:day 2)
                                                         #\Space (:hour 2) #\: (:min 2) #\: (:sec 2))
                                               :timezone tz)))
                              (format nil "~A: ~A" zone-name formatted))
                            (format nil "Unknown timezone '~A'. Try e.g. US/Eastern, Europe/London, Asia/Tokyo" zone-name))))
                  (error ()
                    (format nil "Unknown timezone '~A'. Try e.g. US/Eastern, Europe/London, Asia/Tokyo" zone-name))))))))

(defun github-repo-summary (repo-path)
  "Fetch a GitHub repo summary. REPO-PATH is 'owner/repo'."
  (handler-case
      (trivial-timeout:with-timeout (10)
        (let* ((url (format nil "https://api.github.com/repos/~A" repo-path))
               (raw (drakma:http-request url
                                         :method :get
                                         :additional-headers '(("User-Agent" . "Consort-IRC-Bot/1.0")
                                                               ("Accept" . "application/vnd.github+json"))
                                         :want-stream nil))
               (data (com.inuoe.jzon:parse raw)))
          (let ((name (gethash "full_name" data))
                (desc (gethash "description" data))
                (stars (gethash "stargazers_count" data))
                (lang (gethash "language" data))
                (pushed (gethash "pushed_at" data)))
            (when name
              (format nil "~A~@[ — ~A~] | ~@[~A | ~]★~:D~@[ | pushed ~A~]"
                      name desc lang stars
                      (when pushed (subseq pushed 0 10)))))))
    (error () nil)))

(defplugin gh (plug-request)
  (case (plugin-action plug-request)
    (:docstring "GitHub repo summary. Usage: !gh <owner/repo>")
    (:priority 2.0)
    (:run (let ((repo (second (plugin-token-text-list plug-request))))
            (if (null repo)
                "Usage: !gh <owner/repo>"
                (or (github-repo-summary repo)
                    (format nil "Couldn't find repo '~A'." repo)))))))

;;; ============================================================
;;; Channel quote database (in-memory)
;;; ============================================================

(defvar *quotes* (make-hash-table :test 'equalp :synchronized t)
  "Hash: channel-name → list of (id sender timestamp text).")

(defvar *quote-counter* 0)

(defplugin addquote (plug-request)
  (case (plugin-action plug-request)
    (:docstring "Save a channel quote. Usage: !addquote <text>")
    (:priority 1.5)
    (:run (let* ((tokens (plugin-token-text-list plug-request))
                 (text (format nil "~{~A~^ ~}" (rest tokens)))
                 (channel (plugin-channel-name plug-request))
                 (sender (prefix-nick (parse-prefix (message-prefix (last-message (plugin-conn plug-request)))))))
            (if (str:empty? text)
                "Usage: !addquote <something funny someone said>"
                (let ((id (incf *quote-counter*)))
                  (push (list id sender (get-universal-time) text)
                        (gethash channel *quotes*))
                  (format nil "Quote #~D saved." id)))))))

(defplugin quote (plug-request)
  (case (plugin-action plug-request)
    (:docstring "Recall a random quote, or by number. Usage: !quote [number]")
    (:priority 1.5)
    (:run (let* ((channel (plugin-channel-name plug-request))
                 (quotes (gethash channel *quotes*))
                 (num-str (second (plugin-token-text-list plug-request)))
                 (num (when num-str (parse-integer num-str :junk-allowed t))))
            (cond
              ((null quotes) "No quotes saved yet. Use !addquote to add one.")
              (num
               (let ((q (find num quotes :key #'first)))
                 (if q
                     (format nil "#~D (by ~A): ~A" (first q) (second q) (fourth q))
                     (format nil "No quote #~D." num))))
              (t
               (let ((q (alexandria:random-elt quotes)))
                 (format nil "#~D (by ~A): ~A" (first q) (second q) (fourth q)))))))))

;;; ============================================================
;;; Self-reminders
;;; ============================================================

(defun parse-duration (s)
  "Parse a duration string like '30m', '2h', '1d' into seconds. Returns nil on failure."
  (let ((len (length s)))
    (when (> len 1)
      (let* ((unit (char-downcase (char s (1- len))))
             (num (parse-integer (subseq s 0 (1- len)) :junk-allowed t)))
        (when num
          (case unit
            (#\s num)
            (#\m (* num 60))
            (#\h (* num 3600))
            (#\d (* num 86400))
            (t nil)))))))

(defplugin remind (plug-request)
  (case (plugin-action plug-request)
    (:docstring "Set a reminder. Usage: !remind <duration> <message> (e.g. !remind 30m check the build)")
    (:priority 1.5)
    (:run (let* ((tokens (plugin-token-text-list plug-request))
                 (duration-str (second tokens))
                 (message-words (cddr tokens))
                 (sender (prefix-nick (parse-prefix (message-prefix (last-message (plugin-conn plug-request))))))
                 (conn (plugin-conn plug-request))
                 (channel (plugin-channel-name plug-request)))
            (cond
              ((null duration-str)
               "Usage: !remind <duration> <message> (e.g. !remind 30m check the build)")
              ((null (parse-duration duration-str))
               (format nil "Invalid duration '~A'. Use e.g. 30s, 5m, 2h, 1d" duration-str))
              ((null message-words)
               "What should I remind you about?")
              (t
               (let ((seconds (parse-duration duration-str))
                     (text (format nil "~{~A~^ ~}" message-words)))
                 (bt:make-thread
                  (lambda ()
                    (sleep seconds)
                    (privmsg conn channel
                             (format nil "~A: Reminder — ~A" sender text)))
                  :name (format nil "reminder-~A" sender))
                 (format nil "Got it, I'll remind you in ~A." (format-duration seconds)))))))))

;;; ============================================================
;;; Sandboxed CL evaluator
;;; ============================================================

(defparameter *eval-banned-symbols*
  '(eval load compile open delete-file rename-file ensure-directories-exist
    run-program run-shell-command sb-ext:run-program uiop:run-program
    with-open-file with-open-stream make-instance
    require asdf:load-system ql:quickload
    sb-ext:exit sb-ext:quit
    setf setq defvar defparameter defun defmacro defclass
    in-package delete-package make-package)
  "Symbols that are not allowed in !eval expressions.")

(defun safe-eval-p (form)
  "Check if FORM is safe to evaluate (no banned symbols)."
  (cond
    ((null form) t)
    ((symbolp form)
     (not (member form *eval-banned-symbols* :test #'eq)))
    ((atom form) t)
    ((consp form)
     (and (safe-eval-p (car form))
          (safe-eval-p (cdr form))))
    (t t)))

(defplugin eval (plug-request)
  (case (plugin-action plug-request)
    (:docstring "Evaluate a Common Lisp expression (sandboxed). Usage: !eval <expr>")
    (:priority 3.0)
    (:run (let* ((tokens (plugin-token-text-list plug-request))
                 (expr-str (format nil "~{~A~^ ~}" (rest tokens))))
            (if (str:empty? expr-str)
                "Usage: !eval (+ 1 2)"
                (handler-case
                    (trivial-timeout:with-timeout (5)
                      (let ((form (let ((*read-eval* nil))
                                    (read-from-string expr-str))))
                        (if (safe-eval-p form)
                            (let* ((result (eval form))
                                   (text (format nil "~S" result)))
                              (if (> (length text) 400)
                                  (format nil "~A... [truncated]" (subseq text 0 400))
                                  text))
                            "Expression contains forbidden operations.")))
                  (trivial-timeout:timeout-error ()
                    "Evaluation timed out (5s limit).")
                  (error (e)
                    (format nil "Error: ~A" e))))))))

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

