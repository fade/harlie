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

;; (defun make-webpage-listing-urls (store)
;;   (with-html-output-to-string (s)
;;     (:html
;;      (:head
;;       ;;; this title will have to change if/when we support multiple server connections.
;; ))
;;     s))

(defun make-webpage-listing-urls (store)
  (with-html-output-to-string (s)
    (:html
     (:head
      (:title (escape-string (format nil "Short URL index for server: ~A" *irc-server-name*))))
     (:body
      (:h2 "URL Index")
      (:br)
      (:ul
       (dolist (link (get-urls-and-headlines store))
	 (let ((target (car link))
	       (link-description (str (escape-string (cadr link)))))
	    (htm
	     (:li
	      (:a :href target link-description))))))))
    s))

(defun bug (store)
  (dolist (link (get-urls-and-headlines store))
    ;(format t "~&~A: ~A~&~A: ~A" (type-of (car link)) (car link) (type-of (cadr link)) (cadr link))
    (format t "~%** ~A~%=> ~A~%" (car link) (escape-string (cadr link)))))

(defun html-apology ()
  "Return HTML for a page explaining that a browser has struck out."
  (with-yaclml-output-to-string
    (:html
     (:head
      (:title (escape-string (format nil "You are in a maze of twisty little redirects, all alike.")))
      (:body
       (:p (escape-string  "With apologies" (:br)
			"I don't have that URL" (:br)
			"Perhaps you mistyped?" (:br))))))))

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
