(in-package :harlie)

(defun do-chain-aux (ctx string)
  (chain-in ctx (str:split-omit-nulls #\Space string)))

(defun load-chains-from-file (ctx bookfile)
  "take a text file 'bookfile and load it into the chaining database."
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

;; (defun make-context-table ()
;;   (with-connection (psql-botdb-credentials *bot-config*)
;;     (query "CREATE TABLE contexts (
;;                 context_id bigint NOT NULL,
;;                 context_name text,
;;                 irc_server text,
;;                 irc_channel text,
;;                 web_server text,
;;                 web_port integer,
;;                 web_uri_prefix text);")
;;     (query (format nil "ALTER TABLE contexts OWNER to ~A" (second (psql-botdb-credentials *bot-config*))))
;;     ))

(defun make-context-entry (bot-nick irc-server irc-channel web-server web-port uri-prefix)
  "create a context entry in the database with the data in the lambdalist."
  (with-connection (psql-botdb-credentials *bot-config*)
    (query (format nil "INSERT INTO contexts (
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

(defun initialize-startup-maybe ()
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
          (log:info "Creating tables for the various required users...")
          (make-base-tables db-schema)
          ;; (zero-users)
          ;; harlie-users bot-channels channel-users
          ;; (make-users)
          )))))
