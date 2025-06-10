(in-package :harlie)

(defun do-chain-aux (ctx string)
  (chain-in ctx (str:split-omit-nulls #\Space string)))

(defun load-chains-from-file (ctx bookfile)
  "take a text file 'bookfile and load it into the chaining database for
the appropriate context."
  (let* ((bf (rutils:slurp bookfile))
         (bl (str:split-omit-nulls #\Newline bf)))
    (loop for l in bl
          do (do-chain-aux ctx l))))

(defun fix-bot-herder (sqlfile)
  (let* ((bot-herder (second (psql-botdb-credentials *bot-config*)))
         (file-contents (rutils:slurp sqlfile))
         (updated-contents (cl-ppcre:regex-replace-all "\<bot_runner_uid\>" file-contents bot-herder)))
    (with-open-file (s sqlfile :direction :output :if-exists :supersede)
      (write-sequence updated-contents s))))

(defun load-contexts (&key (bstate *bot-config*) (go? nil))
  "
   context_id | context_name | irc_server  | irc_channel | web_server | web_port | web_uri_prefix 
  ------------+--------------+-------------+-------------+------------+----------+----------------
            1 | chumby       | irc.srh.org | #trinity    | localhost  |     5749 | /
  "  

  (dolist (server-spec (irc-joins bstate))
    (let ((ircserver (car server-spec))
          (web-ports (copy-seq (web-server-ports *bot-config*))))
      (log:debug "~&IRC Server for db context: ~A" ircserver)
      (dolist (nick-spec (cadr server-spec))
        (log:debug "~&[[[ nick-spec:: ~A ]]]" nick-spec)
	(let* ((nickname (car nick-spec))
               (context-name nickname)
               (channels (cadr nick-spec))
               (channame (first channels))
               (web-port (pop web-ports))
               (web-server (web-server-name *bot-config*))
               (web-uri-prefix "/"))
          (log:debug "|||> WHAT IS THE CHANNEL? ~A" channels)
          (log:debug "CONTEXT: IRC handle: ~A on channel:~A" nickname channels)
          (when go?
            (make-context-entry context-name ircserver channame web-server web-port web-uri-prefix :go? go?)
            ))))))

;; (loop with web-ports = (web-server-ports *bot-config*)
;;       for instance in (irc-joins bstate)
;;       for server = (first instance)
;;       for joins = (rest instance)

;;       ;; do (progn
;;       ;;      (terpri) (print server) (terpri)
;;       ;;      (print instance)
;;       ;;      ;; (break)
;;       ;;      )
;;       when (listp joins)
;;         do (loop for context in joins
;;                  do (progn
;;                       (format t "~2&<<< ~A~2%" context)
;;                       (step)
;;                       (let* ((chan-def (caadr context))
;;                              (context-name (first chan-def))
;;                              (channame (caadr chan-def))
;;                              (web-server (web-server-name *bot-config*))
;;                              ;; web-ports must match the number of channel joins.
;;                              (web-port (pop web-ports))
;;                              (web-uri-prefix "/"))
;;                         ;; create the context entry for the given irc-join in *bot-config*
;;                         (break)
;;                         (make-context-entry context-name server channame web-server web-port web-uri-prefix :go? go?)))))

(defun make-context-entry (bot-nick irc-server irc-channel web-server web-port uri-prefix &key (go? t))
  "create a context entry in the database with the data in the lambdalist."
  (with-connection (psql-botdb-credentials *bot-config*)
    (if go?
        (query (format nil "INSERT INTO contexts (
                context_name,
                irc_server,
                irc_channel,
                web_server,
                web_port,
                web_uri_prefix)
VALUES ('~A', '~A', '~A', '~A', '~A', '~A');"
                       bot-nick irc-server irc-channel web-server web-port uri-prefix))
        (format nil "INSERT INTO contexts (
                context_name,
                irc_server,
                irc_channel,
                web_server,
                web_port,
                web_uri_prefix)
VALUES ('~A', '~A', '~A', '~A', '~A', '~A');"
                bot-nick irc-server irc-channel web-server web-port uri-prefix))))

(defun make-base-tables (sqlfile)
  "given the full path to a sql schema template 'SQLFILE for Harlie, fix
the table ownership, and excecute the schema."
  (fix-bot-herder sqlfile)
  (with-connection (psql-botdb-credentials *bot-config*)
    (execute-file sqlfile)))

(defun initialize-startup-maybe (&key  (go? nil))
  (let* ((db-schema (merge-pathnames *here-db* "bot-schema.sql")))
    (handler-case
        (with-connection (psql-botdb-credentials *bot-config*)
          (let* ((person (select-dao 'harlie-user)))
            person))
      (DATABASE-ERROR (e)
        "In this event, the database does not exist, maybe. Try to create it,
       and populate it."
        (progn
          (log:debug "Database probably hasn't been created.. ~A" e)
          (log:info "Creating database to stand up the bot...")
          (uiop:run-program (list "createdb" "botdb") :output t)
          ;; (zero-users)
          ;; harlie-users bot-channels channel-users
          ;; (make-users)
          (log:info "Creating tables for the various required users...")
          (make-base-tables db-schema)
          (load-contexts :bstate *bot-config* :go? t))))))
