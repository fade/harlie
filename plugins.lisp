;;;; plugins.lisp

(in-package #:harlie)

(defvar *plugins* (make-hash-table :test 'equal))

(defclass plugin ()
  ((plugin-name :initarg :plugin-name :accessor plugin-name)
   (plugin-hook :initarg :plugin-hook :accessor plugin-hook)
   (plugin-doc :initform nil :accessor plugin-doc)))

;; *doublehelp* is the fallback plugin which gives documentation on a failed lookup.

(defparameter *doublehelp*
  (make-instance 'plugin
		 :plugin-name "DOUBLEHELP"
		 :plugin-hook (lambda (reply-to token-list)
				(declare (ignore token-list))
				(cond ((eq reply-to :docstring)
				       (list (format nil "Sorry, I don't recognize that command.")
					     (format nil "  Try ~A~A for a list of commands." *url-prefix* "help")))
				      (t nil)))))

(defmacro defplugin (funame args &rest body)
  `(setf (gethash (symbol-name (quote ,funame)) *plugins*)
	 (make-instance 'plugin :plugin-name (symbol-name (quote ,funame))
				:plugin-hook (lambda (,@args) ,@body))))

;; So here's the new drill:
;; plugins are expected to respond to the protocol exemplified in sources.
;; If reply-to is :docstring, then return a documentation string.
;; If reply-to is :priority, then return a float representing the position
;; of this plugin among all the plugins.
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

(defplugin sources (reply-to token-list)
  (declare (ignore token-list))
  (cond ((eq reply-to :docstring)
	 (format nil "Tell us where the source is kept."))
	((eq reply-to :priority) -1.0)
	(t (format nil "git@coruscant.deepsky.com:harlie.git"))))

(defplugin status (reply-to token-list)
  (declare (ignore token-list))
  (cond ((eq reply-to :docstring)
	 (format nil "Give the count of phrases in the chaining database"))
	((eq reply-to :priority) 1.0)
	(t (format nil "I know ~A phrases." (count-phrases)))))

(defplugin conv (reply-to token-list)
  (cond ((eq reply-to :docstring)
	 (format nil "Convert between currencies.  Usage: !conv <amount> <from-abbrev> <to-abbrev>"))
	((eq reply-to :priority) 2.0)
	(t (let* ((amount (second token-list))
		  (from (third token-list))
		  (to (fourth token-list))
		  (forex (find-forex (fetch-formatted-url
				      "http://www.xe.com/ucc/convert/?Amount=~A&From=~A&To=~A"
				      amount from to))))
	     (format nil "~A = ~A" (first forex) (second forex))))))

(defplugin jcw (reply-to token-list)
  (declare (ignore token-list))
  (cond ((eq reply-to :docstring)
	 (format nil "Address the left/right libertarian/asshole continuum"))
	((eq reply-to :priority) 4.0)
	(t (format nil "FUCK YOU, JACKHOLE!"))))

(defplugin rally (reply-to token-list)
  (declare (ignore token-list))
  (cond ((eq reply-to :docstring)
	 (format nil "Deal with the world of racing"))
	((eq reply-to :priority) 4.0)
	(t (format nil "FUCK YOU, HANS!"))))

(defplugin f1 (reply-to token-list)
  (declare (ignore token-list))
  (cond ((eq reply-to :docstring)
	 (format nil "He's such a fucking nancyboy."))
	((eq reply-to :priority) 4.0)
	(t (format nil "FUCK YOU, SCHUMACHER!"))))

(defplugin spew (reply-to token-list)
  (declare (ignore token-list))
  (cond ((eq reply-to :docstring)
	 (format nil "What's the secret word for tonight?"))
	((eq reply-to :priority) -1.0)
	(t (list "I'm a mouthy bastard" "Who can't get everything" "He wants to say" "In one line."))))

(defplugin ccode (reply-to token-list)
  (cond ((eq reply-to :docstring)
	 (format nil "Look up a country code based on a search string.  Usage: !ccode <substring>"))
	((eq reply-to :priority) 3.0)
	(t (let* ((countries (country-lookup (second token-list))))
	     (if (and countries (listp countries))
		 (loop for (a . b) in countries
		       :collect (format nil "[ ~a ][ ~a ]" a b))
		 (format nil "No match for search term: ~A" (second token-list)))))))

(defplugin area (reply-to token-list)
  (cond ((eq reply-to :docstring)
	 (format nil "Look up an area code.  Usage: !area <area code>"))
	((eq reply-to :priority) 3.0)
	(t (let* ((area (areacode-lookup (second token-list))))
	     (if (and area (listp area))
		 (loop for (a . b) in area
		       :collect (format nil "[ ~A ][ ~A ]" a b))
		 (format nil "No area code found for your search term: ~A" (second token-list)))))))

(defplugin iata (reply-to token-list)
  (cond ((eq reply-to :docstring)
	 (format nil "Look up an airport description by its IATA code.  Usage: !iata <airport code>"))
	((eq reply-to :priority) 3.0)
	(t (let ((airports (airport-lookup (second token-list))))
	     (if (and airports (listp airports))
		 (loop for (a . b) in airports
		       :collect (format nil "[ ~A ][ ~A ]" a b))
		 (format nil "No match for your airport: ~A" (second token-list)))))))

(defplugin ciso (reply-to token-list)
  (cond ((eq reply-to :docstring)
	 (format nil "Look up an ISO currency code.  Usage: !ciso <substring>"))
	((eq reply-to :priority) 2.0)
	(t (let ((curr (currency-lookup (second token-list))))
	     (if (and curr (listp curr))
		 (loop for (a . b) in curr
		       :collect (format nil "[ ~A ][ ~A ]" a b))
		 (format nil "~A" curr))))))

;; rate

(defplugin rate (reply-to token-list)
  (cond ((eq reply-to :docstring)
	 (format nil "Look up the conversion rate between two currencies.  Usage: !rate <curr1> <curr2>"))
	((eq reply-to :priority) 2.0)
	(t (let* ((amount 1)
		  (from (second token-list))
		  (to (third token-list))
		  (fx (find-forex (fetch-formatted-url
				   "http://www.xe.com/ucc/convert/?Amount=~A&From=~A&To=~A"
				   amount from to)))
		  (forex (split-sequence #\Space fx)))
	     (format nil "[ ~A ~A ] = [ ~A ~A ]" (first forex) (second forex) (third forex) (fourth forex))))))

(defplugin babble (reply-to token-list)
  (declare (ignore token-list))
  (cond ((eq reply-to :docstring)
	 (format nil "Generate some ill-considered bot spew."))
	((eq reply-to :priority) -1.0)
	(t (format nil "~{~A~^ ~}" (chain)))))

(defplugin haiku (reply-to token-list)
  (declare (ignore token-list))
  (cond ((eq reply-to :docstring)
	 (format nil "each utterance a / closed flower, capable of / independent life."))
	((eq reply-to :priority) 1.5)
	(t (progn
	     (if (not *syllable-counts*) (setf *syllable-counts* (count-syllables)))
	     (format nil "~{~A~^ ~}" (make-haiku))))))

(defplugin ftw (reply-to token-list)
  (declare (ignore token-list))
  (cond ((eq reply-to :docstring)
	 (format nil "How do you like your victory?"))
	((eq reply-to :priority) -1.0)
	(t (format nil "VICTORY!  FLAWLESS!"))))

(defplugin trigger (reply-to token-list)
  (declare (ignore token-list))
  (cond ((eq reply-to :docstring)
	 (format nil "Return the current trigger word list"))
	((eq reply-to :priority) 1.5)
	(t (format nil "~{~A~^, ~}" *trigger-list*))))

(defplugin help (reply-to token-list)
  (cond ((eq reply-to :docstring)
	 (format nil "Advertise how to get help with the bot's commands"))
	((eq reply-to :priority) 1.0)
	((> (length token-list) 1)
	 (funcall
	  (plugin-hook
	   (gethash (string-upcase (remove #\! (second token-list))) *plugins* *doublehelp*))
	  :docstring nil))
	(t (list
	    (format nil "~A~A" *url-prefix* "help")
	    (format nil "  or !help <command>")))))

(defun html-help ()
  "Return HTML for a page giving help with the bot's plugin commands."
  (with-html-output-to-string (s)
    (:html
     (:head
      (:title (str (escape-string (format nil "Bot Help Page")))))
     (:body
      (:h1 (str (escape-string (format nil "Bot Command Help"))))
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
		    (:dt (:b (str (escape-string (format nil "!~A:" (first doc))))))
		    (:dd (str (escape-string (format nil "~A" (second doc))))))
		 (setf oldpriority (third doc))))))))))))


;; ftoc

;; ctof

;; 8ball

;; area

;; calc

;; ===[ hyperspace motivator follows. ]===

(defun plugin-docs ()
  "Generate a list-of-lists from the plugin names, doc strings, and priorities."
  (loop for k being the hash-keys in *plugins*
	collecting (list k
			 (funcall (plugin-hook (gethash k *plugins*)) :docstring nil)
			 (funcall (plugin-hook (gethash k *plugins*)) :priority nil))))

(defun sort-docs (a b)
  "Sort a list-of-lists of plugins by name within priority."
  (cond ((not (= (third a) (third b))) (< (third a) (third b)))
	((not (string= (first a) (first b))) (string< (first a) (first b)))
	(t nil)))

(defun run-plugin (botcmd connection reply-to token-list)
  "Run a plugin's hook function and send the return text back to the requester."
  (let* ((plugname (string-upcase (subseq botcmd 1)))
	 (plugf (gethash plugname *plugins* nil)))
    (if plugf
	(let ((reply (funcall (plugin-hook plugf) reply-to token-list)))
	  (cond ((stringp reply)
		 (qmess connection reply-to
			(format nil "~A:: ~A" (string-downcase plugname) reply)))
		((listp reply)
		 (dolist (line reply)
		   (qmess connection reply-to
			  (format nil "~A:: ~A" (string-downcase plugname) line))))
		(t (qmess connection reply-to
			  (format nil "~A:: I'm a tragic victim of duck typing gone wrong." (string-downcase plugname))))))
	(qmess connection reply-to (format nil "~A: unknown command." (string-downcase plugname))))))
