;;;; web-server.lisp

(in-package #:harlie)

(defparameter *acceptors* nil)

(defun make-webpage-listing-urls (store)
  "Generate HTML for the Web page listing the Web links in the database."
  (let* ((context (make-instance 'bot-context :bot-web-port (acceptor-port (request-acceptor *request*))))
         (bothandle (bot-nick context))
         (channel (bot-irc-channel context)))
    (spinneret:with-html-string
      (:html
       (:head
        (:link :rel "stylesheet" :href "https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.min.css")
        (:title (format nil "URL index for ~A on ~A" bothandle channel)))
       (:body
        (:h2 (format nil "URL index for ~A on ~A" bothandle channel))
        (:br)
        (:ul
         ;; context delimited by web port, so selecting the port here, selects the instance/channel of the bot.
         (dolist (link (get-urls-and-headlines store (make-instance 'bot-context :bot-web-port (acceptor-port (request-acceptor *request*)))))
	   (let ((target (car link))
		 (link-description (cadr link)))
             (:li
              (:a :href target link-description ))))))))))

(defvar +ass-timestamp-format+ '((:YEAR 4) #\- (:MONTH 2) #\- (:DAY 2)))

(defun url-index-ass-dispatch (store &key (rport 5807))
  (let* ((request-port (if (boundp 'hunchentoot:*request*)
                           (acceptor-port (request-acceptor *request*))
                           rport))
         (context (make-instance 'bot-context :bot-web-port request-port)))
    (with-output-to-string (s nil)
      (dolist (link (get-urls-and-headlines store context))
        (let ((target (first link))
              (link-description (second link))
              ;; (who-link (third link))
              (tstamp (local-time:format-timestring nil (fourth link) :format +ass-timestamp-format+)))
          ;; (log:debug "~2& ~A ~A ~A" tstamp target link-description)
          ;;date tab link tab description
          (format s "~&<p>~A~A~A~A~A~%" tstamp #\tab target #\tab link-description))))))

(defun ass-url-index ()
  "Dispatcher for the list of stored web links in the index, returned
according to https://tilde.town/~dzwdz/ass/"
  (let ((uri (request-uri*)))
    (log:debug "~2&/ass/~A" uri)
    (url-index-ass-dispatch *the-url-store*)))

(defun redirect-shortener-dispatch ()
  "Dispatcher for the Web pages served by the bot.
Serve up a redirection, a list of shortened URL links,
or an error message, as appropriate."
  (let ((uri (request-uri*)))
    (if (> (length uri) *how-short*)
	(let* ((short (cond
                        ;; when short links are sent in to the
                        ;; shortener from places like facebook, they
                        ;; add all kinds of tracking spooj in the form
                        ;; of query parameters, which causes a link
                        ;; failure in some cases.
                        ((scan "\\?+" uri)
                         (log:info "~&BELLICOSE QUERY PARAMETERS:: ~A~2%" (split "\\?+" (subseq uri 1)))
                         (car (split "\\?+" (subseq uri 1))))
                        (t (subseq uri 1))))
	       (url (get-url-from-shortstring *the-url-store* short)))
	  (log:info "~2&SHORTTHING: [~A]~2%" short)
	  (if url
	      (redirect url)
	      (let* ((short (subseq uri 4))
		     (url (get-url-from-shortstring *the-url-store* short)))
		(if url
		    (redirect url)
		    (html-apology)))))
	(let ((page (make-webpage-listing-urls *the-url-store*)))
	  (values (format nil "~A" page))))))

(defun html-apology ()
  "Return HTML for a page explaining that a browser has struck out."
  (spinneret:with-html-string
    (:html
     (:head
      (:link :rel "stylesheet" :href "https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.min.css")
      (:title "You are in a maze of twisty little redirects, all alike."))
     (:body
      (:h1
       (:p "With apologies")
       (:p "I don't have that URL...")
       (:p "Perhaps you mistyped?"))))))

(defun help-dispatch ()
  "Dispatcher for the help page served by the bot."
  (html-help))

(defun fetch-file (fname)
  (let ((fpath (merge-pathnames
                fname
                (make-pathname-in-lisp-subdir "harlie/")))
	(s2 (make-string-output-stream)))
    
    (cond ((scan "config.lisp" fname)
	   (format nil "<html><head><title>File Unavailable</title></head><body><p>We're sorry, but that file is not available.</body></html>"))
	  ((scan "[.]lisp|asd$" fname)
	   (let ((url-context (make-instance 'bot-context :bot-web-port (acceptor-port (request-acceptor *request*)))))
	     (setf clhs-lookup::*hyperspec-root* (make-short-url-string url-context "HyperSpec/"))
	     (colorize::colorize-file-to-stream :common-lisp fpath s2 :encoder 'encode-for-tt)
	     (get-output-stream-string s2)))
	  (t
	   (with-open-file (stream (constant-file fpath))
             (do* ((line (read-line stream nil 'eof) (read-line stream nil 'eof))
                   (lines (list line (format nil "<html><head><title>~A</title></head><body><pre>" fname))
                          (if (not (eq 'eof line))
                              (cons (escape-string line) lines)
                              lines)))
                  ((eq line 'eof) (format nil "~{~A~^~%~}~%</pre></body></html>~%" (reverse lines)))))))))

(defun fetch-source-dir ()
  (let ((fotchery
	  (directory (make-pathname-in-lisp-subdir "harlie/*.*")))
	(url-context
	  (make-instance 'bot-context :bot-web-port (acceptor-port (request-acceptor *request*)))))
    (format nil "~{~A~^~%~}~%"
	    (append
	     '("<html><head><title>Bot Source</title></head><body><ul>")
	     (mapcar
	      #'(lambda (x)
		  (let ((fname (subseq (scan-to-strings "[/]([^/]*)$" (unix-pathstring-from-pathname x)) 1))) 
		    (format nil "<li><a href=\"~A~A\"><h2>~A</h2></a></li>~%"
			    (make-short-url-string url-context "source/") fname fname)))
	      (remove-if
	       #'(lambda (x)
		   (scan "config.lisp|[/][.]|[/]$|[.]fasl$|^[#]|[#]$"
			 (unix-pathstring-from-pathname x)))
	       fotchery)) 
	     '("</ul></body></html>")))))

(defun source-dispatch ()
  "Dispatcher for a brane-dead file server out of the bot."
  (log:info "~A" (request-uri*))
  (if (not (scan (create-caseless-scanner "^/source/?$") (request-uri*)))
      (fetch-file (subseq (request-uri*) 8))
      (fetch-source-dir)))

(defun hyperspec-base-dispatch ()
  "Dispatcher for the index page of the HyperSpec"
  (let* ((url-context (make-instance 'bot-context :bot-web-port (acceptor-port (request-acceptor *request*)))))
    (redirect (make-short-url-string url-context "HyperSpec/Front/index.htm"))))

(defun gitrepo-base-dispatch ()
  "Dispatcher for the Git repo."
  (shell-out (make-pathname-in-lisp-subdir "harlie/git-update-server-info.sh"))
  (let* ((url-context (make-instance 'bot-context :bot-web-port (acceptor-port (request-acceptor *request*)))))
    (redirect (concatenate 'string (make-short-url-string url-context "gitrepo/") (subseq (request-uri*) 12)))))

(defun glom-on-prefix (s thunk)
  (push (create-prefix-dispatcher s thunk) *dispatch-table*))

(defun glom-on-regex (s thunk)
  (push (create-regex-dispatcher (create-caseless-scanner s) thunk) *dispatch-table*))

(defun glom-on-static-file (s p)
  (push (create-static-file-dispatcher-and-handler s (make-pathname-in-lisp-subdir p)) *dispatch-table*))
(defun glom-on-folder (s p)
  (push (create-folder-dispatcher-and-handler s (make-pathname-in-lisp-subdir p)) *dispatch-table*))

(defun start-web-servers ()
  "Initialize and start the web server subsystem."
  (setf clhs-lookup::*hyperspec-map-file*
	(make-pathname-in-lisp-subdir "HyperSpec/Data/Map_Sym.txt"))
  (dolist (port (web-server-ports *bot-config*))
    (push (make-instance 'hunchentoot:easy-acceptor
			 :port port
			 :access-log-destination (make-pathname-in-lisp-subdir "harlie/logs/http-access.log")
			 :message-log-destination (make-pathname-in-lisp-subdir "harlie/logs/http-error.log"))
	  *acceptors*)
    (glom-on-prefix "/" 'redirect-shortener-dispatch)
    ;; (glom-on-prefix "/ass" 'url-index-ass-dispatch)
    (glom-on-prefix "/ass/" 'ass-url-index)
    (glom-on-static-file "/robots.txt" "harlie/robots.txt")
    (glom-on-regex "^/help/?$" 'help-dispatch)
    (glom-on-regex "^/source" 'source-dispatch)
    (glom-on-regex "^/hyper(spec)?/?$" 'hyperspec-base-dispatch)
    (glom-on-folder "/HyperSpec/" "HyperSpec/")
    (glom-on-folder "/gitrepo/" "harlie/.git/")
    (glom-on-prefix "/harlie.git" 'gitrepo-base-dispatch)
    (start (car *acceptors*))))

(defun stop-web-servers ()
  "Shut down the web server subsystem."
  (dolist (acceptor *acceptors*) (hunchentoot:stop acceptor))
  (setf *acceptors* nil)
  (setf *dispatch-table* (list 'dispatch-easy-handlers 'default-dispatcher)))
