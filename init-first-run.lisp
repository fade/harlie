(in-package :harlie)

(defun do-chain-aux (ctx string)
  (chain-in ctx (str:split-omit-nulls #\Space string)))

(defun load-chains-from-file (ctx bookfile)
  "take a text file 'bookfile and load it into the chaining database for
the appropriate context."
  (let* ((bf (rutils:slurp bookfile))
         (bl (str:split-omit-nulls #\Full_stop bf)))
    (loop for l in bl
          do (do-chain-aux ctx l))))

(defun fix-bot-herder (sqlfile)
  (let* ((bot-herder (second (db-credentials *bot-config*)))
         (file-contents (rutils:slurp sqlfile))
         (updated-contents (cl-ppcre:regex-replace-all "\<bot_runner_uid\>" file-contents bot-herder)))
    (with-open-file (s sqlfile :direction :output :if-exists :supersede)
      (write-sequence updated-contents s))))

(defun make-context-entry (bot-nick irc-server irc-channel web-server web-port uri-prefix)
  "Upsert one row into the contexts table using parameterised queries.
Safe to call on every startup; the ON CONFLICT clause makes it idempotent."
  (execute
   "INSERT INTO contexts (context_name, irc_server, irc_channel,
                          web_server, web_port, web_uri_prefix)
    VALUES ($1, $2, $3, $4, $5, $6)
    ON CONFLICT (context_name, irc_server, irc_channel)
    DO UPDATE SET web_server     = EXCLUDED.web_server,
                  web_port       = EXCLUDED.web_port,
                  web_uri_prefix = EXCLUDED.web_uri_prefix"
   bot-nick irc-server irc-channel web-server web-port uri-prefix))

(defun load-contexts ()
  "Upsert one contexts row per connection-spec in *BOT-CONFIG*.
Idempotent — safe to call on every startup so the table stays in sync
with the config without requiring manual SQL."
  (with-connection (db-credentials *bot-config*)
    (dolist (cs (connections *bot-config*))
      (log:debug "~&CONTEXT: nick=~A server=~A channel=~A port=~A"
                 (cs-nick cs) (cs-server cs) (cs-channel cs) (cs-web-port cs))
      (make-context-entry
       (cs-nick cs)
       (cs-server cs)
       (cs-channel cs)
       (web-server-name *bot-config*)
       (cs-web-port cs)
       "/"))))

(defun make-base-tables (sqlfile)
  "given the full path to a sql schema template 'SQLFILE for Harlie, fix
the table ownership, and excecute the schema."
  (fix-bot-herder sqlfile)
  (with-connection (db-credentials *bot-config*)
    (execute-file sqlfile)))

(defun initialize-startup-maybe (&key (go? nil))
  (let* ((db-schema (merge-pathnames *here-db* "bot-schema.sql")))
    (handler-case
        (with-connection (db-credentials *bot-config*)
          (select-dao 'harlie-user))
      (DATABASE-ERROR (e)
        (log:debug "Database probably hasn't been created.. ~A" e)
        (log:info "Creating database to stand up the bot...")
        (uiop:run-program (list "createdb" (first (db-credentials *bot-config*))) :output t)
        (log:info "Creating tables for the various required users...")
        (make-base-tables db-schema)))
    ;; Sync contexts table from config on every startup.
    (when go?
      (load-contexts))))
