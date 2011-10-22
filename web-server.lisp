;;;; web-server.lisp

(in-package #:harlie)

(defparameter *acceptors* nil)

;; (defun make-webpage-listing-urls (store)
;;   "Generate and return the HTML for a page listing all the URLS in the store."
;;   (concatenate 'string "<html><head><title>Bot Spew</title></head><body><ul>"
;; 	       (apply 'concatenate 'string
;; 		      (mapcar (lambda (x) (format nil "<li><a href=\"~A\">~A</A></li>" (car x) (cadr x)))
;; 			      (get-urls-and-headlines store)))
;; 	       "</ul></body></html>"))

(defun make-webpage-listing-urls (store)
  (with-yaclml-output-to-string
    (<:html
     (<:head
      (<:title "Bot Spew")
      (<:body
       (<:ul
	(dolist (link (get-urls-and-headlines store))
	  (<:li
	   (<:a :href (car link) :name (format nil "~A"
					       (cl-who:escape-string (cadr link))))))))))))

(defun html-apology ()
  "Return HTML for a page explaining that a browser has struck out."
  (format nil "
<html>
<head>
<title>You are in a maze of twisty little redirects, all alike</title>
</head>
<body>
<center>
<p>With apologies<br>
I don't have that URL<br>
Perhaps you mistyped?<br>
</p>
</center>
</body>
</html>"))

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
	  (format t "~&passed.. prepare to get a page!~&~A" page)
	  (values (format nil "~A" page)))))) ;;(format nil "<html><head><title>I am a constant webpage</title></head></html>")

(defun start-web-servers ()
  (push (make-instance 'hunchentoot:acceptor :port *web-server-port*) *acceptors*)
  (hunchentoot:start (car *acceptors*))
  (push (create-prefix-dispatcher "/" 'redirect-shortener-dispatch) *dispatch-table*))

(defun stop-web-servers ()
  (dolist (acceptor *acceptors*) (hunchentoot:stop acceptor))
  (setf *acceptors* nil)
  (setf *dispatch-table* (list 'dispatch-easy-handlers 'default-dispatcher)))
