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

;; (defun metar-units-symbol (s)
;;   "Return the temperature-scale-name symbol corresponding to the specified string."
;;   (cond ((scan "^[kK]" s) :Kelvin)
;; 	((scan "^[fFiI]" s) :Fahrenheit)
;; 	(t :Centigrade)))

(defun form-metar-query-string (location)
  (format nil
	  (format nil "~{~A~^&~}"
		  '("https://aviationweather.gov/cgi-bin/data/metar.php?ids=\~\A&hours=0&format=json"
		    "std_trans=standard"
		    "chk_metars=on"
		    "hoursStr=most+recent+only"
		    "submitmet=Submit")) location))

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
            ;; FIXME: if we pass a bad code to the remote, we get a
            ;; null return in the form of an empty vector. we need to
            ;; test for that.
	    (metar-data (svref (com.inuoe.jzon:parse (http-request (form-metar-query-string location)
                                                                   :preserve-uri t :redirect 16)) 0)))
       (if metar-data
	   (multiple-value-bind (station-name time-string windspeed cur-temp dew-temp) (metar-extract-data metar-data)
	     (if (and station-name time-string windspeed cur-temp dew-temp)
		 (let ((windchill (calculate-wind-chill cur-temp windspeed))
		       (humidex (calculate-humidex cur-temp dew-temp)))
		   (apply #'format nil "~A ~A   Current temperature ~A~@[, wind chill ~A~]~@[, humidex ~A~], dewpoint ~A"
			  station-name time-string
			  (mapcar #'(lambda (x) (metar-temp-value x units)) (list cur-temp windchill humidex dew-temp))))
		 (format nil "Sorry, no weather at ~A." location)))
	   (format nil "Sorry, I don't know from ~A." location))))))

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

(defplugin pope (plug-request)
  (case (plugin-action plug-request)
    (:docstring (format nil "Report the head of the Roman church."))
    (:priority 2.0)
    (:run (extract-from-html (chtml:parse (webget "http://www.vatican.va/phome_en.htm")
					  (chtml:make-lhtml-builder))
			     'papal-anchor
			     'papal-extractor))))

(defplugin roll (plug-request)
  (case (plugin-action plug-request)
    (:docstring (format nil "Roll N of P polyhedral dice and return the sum."))
    (:priority 3.0)
    (:run (let* ((n (parse-integer (second (plugin-token-text-list plug-request)) :junk-allowed t))
                 (p (parse-integer (third (plugin-token-text-list plug-request)) :junk-allowed t))
                 (v (loop for dice from 1 to n
                          for roll = (random p)
                          summing roll into total-roll
                          finally (return total-roll))))
            (format nil "You roll ~A d~A... the roll is ~:d!" n p v)))))


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
