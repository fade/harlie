;;;; web-server.lisp

(in-package #:harlie)

(defparameter *acceptors* nil)

(defun make-webpage-listing-urls (store)
  "Generate HTML for the Web page listing the Web links in the database."
  (with-html-output-to-string (s)
    (:html
     (:head
      (:title (str (escape-string (format nil "Short URL index for server: ~A" (config-irc-server-name *bot-config*))))))
     (:body
      (:h2 "URL Index")
      (:br)
      (:ul
       (dolist (link (get-urls-and-headlines store))
	 (let ((target (car link))
	       (link-description (cadr link)))
	   (htm
	    (:li
	     (:a :href target (str (escape-string link-description))))))))))
    s))

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
  (format t "~&In redirect-shortener-dispatch")
  (let ((uri (request-uri*)))
    (if (> (length uri) *how-short*)
	(let* ((short (subseq (request-uri*) 1))
	       (url (get-url-from-shortstring *the-url-store* short)))
	  
	  (if url
	      (redirect url)
	      (let* ((short (subseq (request-uri*) 4))
		     (url (get-url-from-old-shortstring *pomo-url-store* short)))
		(if url
		    (redirect url)
		    (html-apology)))))
	(let (( page (make-webpage-listing-urls *the-url-store*)))
	  ;; (format t "~&passed.. prepare to get a page!~&~A" page)
	  (values (format nil "~A" page))))))

(defun redirect-help-dispatch ()
  "Dispatcher for the help page served by the bot."
  (html-help))

(defun start-web-servers ()
  "Initialize and start the web server subsystem."
  (push (make-instance 'hunchentoot:acceptor :port (config-web-server-port *bot-config*)) *acceptors*)
  (hunchentoot:start (car *acceptors*))
  (push (create-prefix-dispatcher "/" 'redirect-shortener-dispatch) *dispatch-table*)
  (push (create-prefix-dispatcher "/help" 'redirect-help-dispatch) *dispatch-table*))

(defun stop-web-servers ()
  "Shut down the web server subsystem."
  (dolist (acceptor *acceptors*) (hunchentoot:stop acceptor))
  (setf *acceptors* nil)
  (setf *dispatch-table* (list 'dispatch-easy-handlers 'default-dispatcher)))
