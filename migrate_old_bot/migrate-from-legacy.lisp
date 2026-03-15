;;;; migrate-from-legacy.lisp
;;;;
;;;; Load this file into a RUNNING legacy Harlie image via Slynk or any
;;;; connected REPL.  The harlie package, the legacy config class, and all
;;;; dependencies are already present in the live image.  Then call
;;;; (harlie::run-lisp-migration) to produce new-config.lisp.
;;;;
;;;; Procedure:
;;;;   1. Connect to the running legacy bot via Slynk.
;;;;   2. (load "/path/to/migrate-from-legacy.lisp")
;;;;   3. (harlie::run-lisp-migration)   ; review printed output
;;;;   4. Stop the bot.
;;;;   5. bash migrate.sh                ; database migration
;;;;   6. Deploy new code and restart.
;;;;
;;;; Author: Brian O'Reilly

(in-package :harlie)

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
  "Zip FLAT-SPECS with the first (length FLAT-SPECS) entries of WEB-SERVER-PORTS.

The legacy load-contexts consumed ports with (pop web-ports) inside a loop
over nick-specs, so a config with more ports than active nick-specs was
valid — surplus ports were silently unused.  We replicate that behaviour:
require at least as many ports as nick-specs, use the first N, and warn
about any remainder.

Signals an error only if there are FEWER ports than nick-specs."
  (let ((nspecs (length flat-specs))
        (nports (length web-server-ports)))
    (when (< nports nspecs)
      (error "~&Migration error: ~A nick-spec~:P but only ~A web-server-port~:P.~%~
              There must be at least one port per nick-spec.~%~
              Nick-specs found:~%~{  ~A~%~}"
             nspecs nports
             (mapcar (lambda (s) (format nil "~A on ~A" (getf s :nick) (getf s :server)))
                     flat-specs)))
    (when (> nports nspecs)
      (format t "~&Note: ~A web-server-port~:P, ~A nick-spec~:P — ~
                 ~A surplus port~:P (~{~A~^, ~}) will not appear in the ~
                 new config.~%"
              nports nspecs (- nports nspecs)
              (nthcdr nspecs web-server-ports))))
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

(defun run-lisp-migration (&key (output-file (merge-pathnames "new-config.lisp" *syspath*))
                                (config *bot-config*))
  "Top-level migration entry point.  Run from within a live legacy Harlie image.

Validates that CONFIG is a legacy config, translates it to the new
connection-spec format, writes the result to OUTPUT-FILE, and prints a
step-by-step operator checklist."
  (format t "~&=== Harlie legacy config migration ===~2%")

  (unless (legacy-config-p config)
    (error "~&*bot-config* does not appear to be a legacy config ~
            (no irc-joins slot found).~%~
            This utility must be run from within a legacy Harlie image."))

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
  (format t "  2. Stop this bot instance.~%")
  (format t "  3. Copy ~A into place as config.lisp.~%" output-file)
  (format t "  4. Run the database migration:~%")
  (format t "       bash migrate.sh~%")
  (format t "     This backs up the database, fixes any corrupted irc_server~%")
  (format t "     values, deduplicates contexts rows, and adds the unique~%")
  (format t "     constraint required by the new startup sequence.~%")
  (format t "  5. Check out or update to the new code~%")
  (format t "     (branch: unfuck_configuration_and_state).~%")
  (format t "  6. Start the bot normally.  load-contexts will sync the~%")
  (format t "     contexts table from config on every startup.~%")

  (values))
