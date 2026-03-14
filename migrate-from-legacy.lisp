;;;; migrate-from-legacy.lisp
;;;;
;;;; Self-contained migration utility.  Load this file FIRST, before
;;;; config.lisp.  It defines the harlie package, the legacy 13-slot config
;;;; class, and make-config — so that config.lisp evaluates cleanly even
;;;; without the full system loaded.  Then call (harlie::run-lisp-migration)
;;;; to produce a new-config.lisp ready to drop in as config.lisp.
;;;;
;;;; Usage:
;;;;   sbcl --load migrate-from-legacy.lisp \
;;;;        --load config.lisp \
;;;;        --eval '(harlie::run-lisp-migration)' \
;;;;        --quit
;;;;
;;;; Author: Brian O'Reilly

;;; ---------------------------------------------------------------------------
;;; Package — define minimally if not already present
;;; ---------------------------------------------------------------------------

(unless (find-package :harlie)
  (defpackage :harlie
    (:use :common-lisp)
    (:export #:run-lisp-migration)))

(in-package :harlie)

;;; ---------------------------------------------------------------------------
;;; Legacy config class — mirrors the old 13-slot definition so that
;;; config.lisp's make-config call succeeds when loaded standalone.
;;; Only defined if the class is not already present (idempotent).
;;; ---------------------------------------------------------------------------

(unless (find-class 'legacy-config nil)
  (defclass legacy-config ()
    ((irc-server-name          :initarg :irc-server-name)
     (irc-channel-names        :initarg :irc-channel-names)
     (irc-joins                :initarg :irc-joins                :initform nil)
     (ssl                      :initarg :ssl                      :initform :none)
     (irc-nickchannels         :initarg :irc-nickchannels)
     (web-server-name          :initarg :web-server-name)
     (web-server-ports         :initarg :web-server-ports)
     (url-store-type           :initarg :url-store-type)
     (psql-old-credentials     :initarg :psql-old-credentials     :initform nil)
     (psql-url-new-credentials :initarg :psql-url-new-credentials :initform nil)
     (psql-chain-credentials   :initarg :psql-chain-credentials   :initform nil)
     (psql-context-credentials :initarg :psql-context-credentials :initform nil)
     (psql-botdb-credentials   :initarg :psql-botdb-credentials   :initform nil))))

;;; make-config: create a legacy-config instance.
;;; Defined only if not already bound so this file is safe to load into a
;;; fully-loaded legacy image where make-config already exists.
(unless (fboundp 'make-config)
  (defun make-config (&rest args)
    (let ((instance (make-instance 'legacy-config)))
      (loop for (key val) on args by #'cddr
            for slot = (case key
                         (:irc-server-name          'irc-server-name)
                         (:irc-channel-names        'irc-channel-names)
                         (:irc-joins                'irc-joins)
                         (:ssl                      'ssl)
                         (:irc-nickchannels         'irc-nickchannels)
                         (:web-server-name          'web-server-name)
                         (:web-server-ports         'web-server-ports)
                         (:url-store-type           'url-store-type)
                         (:psql-old-credentials     'psql-old-credentials)
                         (:psql-url-new-credentials 'psql-url-new-credentials)
                         (:psql-chain-credentials   'psql-chain-credentials)
                         (:psql-context-credentials 'psql-context-credentials)
                         (:psql-botdb-credentials   'psql-botdb-credentials))
            when slot
              do (setf (slot-value instance slot) val))
      instance)))

;;; Stub out other symbols config.lisp may reference at load time.
(defvar *bot-config* nil)
(defvar *bot-database-credentials* nil)
(defvar *threads* 20)
(defvar *ignore-phrase* "NOTIFY:: Help, I'm a bot!")
(defvar *binary-url-suffixes* nil)
(defvar *user-agents* nil)
(defvar *twitter-auth* nil)
(defvar *trig* nil)
(defvar *syspath* nil)
(defvar *here-db* nil)

;;; Silence calls that config.lisp may make at load time
(unless (fboundp 'log:config)
  (defun log:config (&rest args) (declare (ignore args))))

;;; ---------------------------------------------------------------------------
;;; Slot readers
;;; ---------------------------------------------------------------------------

(defun legacy-slot (config slot-name)
  "Read SLOT-NAME from CONFIG via slot-value.  Returns nil if absent."
  (when (slot-exists-p config slot-name)
    (slot-value config slot-name)))

(defun legacy-irc-joins (config)
  (legacy-slot config 'irc-joins))

(defun legacy-web-server-ports (config)
  (legacy-slot config 'web-server-ports))

(defun legacy-db-credentials (config)
  "Return credentials from whichever legacy slot is populated."
  (or (legacy-slot config 'psql-botdb-credentials)
      (legacy-slot config 'psql-context-credentials)))

(defun legacy-web-server-name (config)
  (legacy-slot config 'web-server-name))

(defun legacy-config-p (config)
  "Return true if CONFIG looks like a legacy 13-slot config instance."
  (slot-exists-p config 'irc-joins))

;;; ---------------------------------------------------------------------------
;;; Structural conversion
;;; ---------------------------------------------------------------------------

(defun flatten-legacy-nick-specs (irc-joins)
  "Walk the nested irc-joins structure and return a flat list of plists.

irc-joins structure:
  ( server-group... )
  server-group  = (server-spec nick-specs-list)
  server-spec   = (\"hostname\" :ssl-keyword-or-nil)
  nick-spec     = (nick (channel [channel-key]) [nickserv-password])

Returns a list of property lists with keys:
  :server :ssl :nick :channel :channel-key :nickserv-password"
  (loop for server-group in irc-joins
        for server-spec      = (first server-group)
        for hostname         = (first server-spec)
        for ssl-keyword      = (second server-spec)
        for ssl              = (member ssl-keyword '(:ssl :tls))
        nconcing
        (loop for nick-spec in (second server-group)
              for nick              = (first nick-spec)
              for channel-list      = (second nick-spec)
              for channel           = (first channel-list)
              for channel-key       = (second channel-list)
              for nickserv-password = (third nick-spec)
              collect (list :server            hostname
                            :ssl               (if ssl t nil)
                            :nick              nick
                            :channel           channel
                            :channel-key       channel-key
                            :nickserv-password nickserv-password))))

(defun pair-with-ports (flat-specs web-server-ports)
  "Zip FLAT-SPECS with WEB-SERVER-PORTS.
Signals an error with a clear message if the counts do not match."
  (let ((nspecs (length flat-specs))
        (nports (length web-server-ports)))
    (unless (= nspecs nports)
      (error "~&Migration error: ~A nick-spec~:P but ~A web-server-port~:P.~%~
              web-server-ports must have exactly one entry per nick-spec ~
              across all servers.~%~
              Nick-specs found:~%~{  ~A~%~}"
             nspecs nports
             (mapcar (lambda (s) (format nil "~A on ~A" (getf s :nick) (getf s :server)))
                     flat-specs))))
  (mapcar (lambda (spec port)
            (append spec (list :web-port port)))
          flat-specs web-server-ports))

;;; ---------------------------------------------------------------------------
;;; Printing
;;; ---------------------------------------------------------------------------

(defun print-connection-spec-form (spec &optional (stream *standard-output*))
  "Print a single make-connection-spec form for SPEC (a plist).
Optional fields are omitted when nil."
  (format stream "    (make-connection-spec~%")
  (format stream "     :server ~S :ssl ~A~%"
          (getf spec :server)
          (if (getf spec :ssl) "t" "nil"))
  (format stream "     :nick ~S :channel ~S~%"
          (getf spec :nick)
          (getf spec :channel))
  (when (getf spec :channel-key)
    (format stream "     :channel-key ~S~%" (getf spec :channel-key)))
  (when (getf spec :nickserv-password)
    (format stream "     :nickserv-password ~S~%" (getf spec :nickserv-password)))
  (format stream "     :web-port ~A)" (getf spec :web-port)))

(defun migrate-legacy-config (&key (config *bot-config*)
                                   (stream *standard-output*)
                                   (var-name "*bot-config*"))
  "Translate the legacy CONFIG object into a new-style make-config form
and write it to STREAM.  Output is valid Lisp; paste into config.lisp."
  (let* ((irc-joins  (legacy-irc-joins config))
         (ports      (legacy-web-server-ports config))
         (flat-specs (flatten-legacy-nick-specs irc-joins))
         (specs      (pair-with-ports flat-specs ports))
         (db-creds   (legacy-db-credentials config))
         (web-name   (legacy-web-server-name config)))
    (format stream "~&;;; Generated by migrate-from-legacy.lisp ~A~%"
            (multiple-value-bind (s m h d mo y) (decode-universal-time (get-universal-time))
              (format nil "~4D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D" y mo d h m s)))
    (format stream "(defparameter ~A~%" var-name)
    (format stream "  (make-config~%")
    (format stream "   :db-credentials  ~S~%" db-creds)
    (format stream "   :web-server-name ~S~%" web-name)
    (format stream "   :connections~%")
    (format stream "   (list~%")
    (loop for spec in specs
          for i from 1
          do (print-connection-spec-form spec stream)
             (if (< i (length specs))
                 (format stream "~%")
                 (format stream ")))~%")))
    (values)))

;;; ---------------------------------------------------------------------------
;;; Orchestrator
;;; ---------------------------------------------------------------------------

(defun run-lisp-migration (&key (output-file "new-config.lisp")
                                (config *bot-config*))
  "Top-level migration entry point.

Validates that CONFIG is a legacy Harlie config, translates it to the
new connection-spec format, writes the result to OUTPUT-FILE, and prints
a step-by-step operator checklist."
  (format t "~&=== Harlie legacy config migration ===~2%")

  (unless (legacy-config-p config)
    (error "~&*bot-config* does not appear to be a legacy config ~
            (no irc-joins slot found).~%~
            Ensure migrate-from-legacy.lisp is loaded BEFORE config.lisp."))

  (let ((irc-joins (legacy-irc-joins config)))
    (unless irc-joins
      (error "~&irc-joins slot is nil — nothing to migrate.")))

  (format t "Translating *bot-config* (~A server group~:P)...~%"
          (length (legacy-irc-joins config)))

  (with-open-file (out output-file
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (migrate-legacy-config :config config :stream out))

  (format t "~%--- begin ~A ---~%" output-file)
  (migrate-legacy-config :config config :stream *standard-output*)
  (format t "~%--- end ~A ---~2%" output-file)

  (format t "~&Written to: ~A~2%" (truename output-file))

  (format t "~&Next steps:~%")
  (format t "  1. Review ~A carefully.~%" output-file)
  (format t "     Verify each connection-spec has the right server, nick,~%")
  (format t "     channel, ssl flag, and web-port.~%")
  (format t "  2. Copy it into place as config.lisp.~%")
  (format t "  3. Run the database migration:~%")
  (format t "       bash migrate.sh~%")
  (format t "     This backs up the database, fixes any corrupted irc_server~%")
  (format t "     values, deduplicates contexts rows, and adds the unique~%")
  (format t "     constraint required by the new startup sequence.~%")
  (format t "  4. Check out or update to the new code~%")
  (format t "     (branch: unfuck_configuration_and_state).~%")
  (format t "  5. Start the bot normally.  load-contexts will sync the~%")
  (format t "     contexts table from config on every startup.~%")

  (values))
