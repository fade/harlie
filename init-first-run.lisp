(in-package :harlie)

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
          ;; (make-base-tables db-schema)
          ;; (zero-users)
          ;; harlie-users bot-channels channel-users
          ;; (make-users)
          (make-base-tables db-schema)
          )))))
