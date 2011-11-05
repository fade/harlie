;;;; web-server.lisp

(in-package #:harlie)

(defparameter *acceptors* nil)

(defun make-webpage-listing-urls (store)
  "Generate HTML for the Web page listing the Web links in the database."
  (let ((context (make-instance 'bot-context :bot-web-port (acceptor-port (request-acceptor *request*)))))
    (with-html-output-to-string (s)
      (:html
       (:head
	(:title (str (escape-string (format nil "Short URL index for ~A on ~A" (bot-nick context) (bot-irc-channel context))))))
       (:body
	(:h2 "URL Index")
	(:br)
	(:ul
	 (dolist (link (get-urls-and-headlines store (make-instance 'bot-context :bot-web-port (acceptor-port (request-acceptor *request*)))))
	   (let ((target (car link))
		 (link-description (cadr link)))
	     (htm
	      (:li
	       (:a :href target (str (escape-string link-description))))))))))
      s)))

(defun bug (store)
  (dolist (link (get-urls-and-headlines store))
    ;(format t "~&~A: ~A~&~A: ~A" (type-of (car link)) (car link) (type-of (cadr link)) (cadr link))
    (format t "~%** ~A~%=> ~A~%" (car link) (escape-string (cadr link)))))

(defun html-apology ()
  "Return HTML for a page explaining that a browser has struck out."
  (with-html-output-to-string (s)
    (:html
     (:head
      (:title (str
	       (escape-string (format nil "You are in a maze of twisty little redirects, all alike.")))))
     (:body
      (:h1
       (htm
       (:p "With apologies")
       (:p "I don't have that URL...")
       (:p "Perhaps you mistyped?")))))
    s))

(defun redirect-shortener-dispatch ()
  "Dispatcher for the Web pages served by the bot.
Serve up a redirect page, a list of shortened URL links,
or an error message, as appropriate."
  (let ((uri (request-uri*)))
    (if (> (length uri) *how-short*)
	(let* ((short (subseq (request-uri*) 1))
	       (url (get-url-from-shortstring *the-url-store* short)))
	  
	  (if url
	      (redirect url)
	      (let* ((short (subseq (request-uri*) 4))
		     (url (get-url-from-shortstring *the-url-store* short)))
		(if url
		    (redirect url)
		    (html-apology)))))
	(let (( page (make-webpage-listing-urls *the-url-store*)))
	  ;; (format t "~&passed.. prepare to get a page!~&~A" page)
	  (values (format nil "~A" page))))))

(defun redirect-help-dispatch ()
  "Dispatcher for the help page served by the bot."
  (html-help))

(defun fotch-file (fname)
  (let ((fpath (merge-pathnames fname
				 (merge-pathnames
				  "SourceCode/lisp/harlie/"
				  (make-pathname
				   :directory
				   (pathname-directory (user-homedir-pathname))))))
	(gfulg nil)
	)
    (with-open-file (stream (constant-file fpath))
      (do* ((line (read-line stream) (read-line stream nil 'eof))
	    (lines (list (format nil "<html><head><title>~A</title></head><body><pre>~%" fname)) (cons (format nil "~A~%" line) lines)))
	   ((eq line 'eof) (setf gfulg (apply 'concatenate 'string (reverse (cdr lines))))))
      (setf gfulg (format nil "~A</pre></body></html>" gfulg)) 
      (format t "~A" gfulg)
      gfulg)))

(defun fotch-source-dir ()
  (let ((fotchery (directory
		   (merge-pathnames
		    "SourceCode/lisp/harlie/*.*"
		    (make-pathname
		     :directory
		     (pathname-directory (user-homedir-pathname))))))
	(url-context (make-instance 'bot-context :bot-web-port (acceptor-port (request-acceptor *request*)))))
    (format nil "~{~A~^~%~}~%"
	    (append
	     '("<html><head><title>Bot Source</title></head><body><ul>")
	     (mapcar
	      #'(lambda (x)
		  (let ((fname (subseq (scan-to-strings "[/]([^/]*)$" (sb-ext:native-namestring x)) 1))) 
		    (format nil "<li><a href=\"~A~A\"><h2>~A</h2></a></li>~%"
			    (make-short-url-string url-context "source/") fname fname)))
	      (remove-if
	       #'(lambda (x)
		   (scan "[/][.]|[/]$"
			 (sb-ext:native-namestring x)))
	       fotchery)) 
	     '("</ul></body></html>")))))

(defun redirect-source-dispatch ()
  "Dispatcher for a brane-dead file server out of the bot."
  (if (not (string= (request-uri*) "/source/")) 
      (fotch-file (subseq (request-uri*) 8))
      (fotch-source-dir)))

(defun start-web-servers ()
  "Initialize and start the web server subsystem."
  (dolist (port (config-web-server-ports *bot-config*))
    (push (make-instance 'hunchentoot:acceptor :port port) *acceptors*)
    (push (hunchentoot:create-prefix-dispatcher "/" 'redirect-shortener-dispatch) *dispatch-table*)
    (push (hunchentoot:create-prefix-dispatcher "/help" 'redirect-help-dispatch) *dispatch-table*)
    (push (hunchentoot:create-prefix-dispatcher "/source/" 'redirect-source-dispatch) *dispatch-table*)
    (hunchentoot:start (car *acceptors*))))

(defun stop-web-servers ()
  "Shut down the web server subsystem."
  (dolist (acceptor *acceptors*) (hunchentoot:stop acceptor))
  (setf *acceptors* nil)
  (setf *dispatch-table* (list 'dispatch-easy-handlers 'default-dispatcher)))
