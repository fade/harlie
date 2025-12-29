;;; users.lisp --- 
;; 

(in-package :harlie)

(defclass bot-channel ()
  ((bot-channel-id :col-type serial
                   :reader bot-channel-id)
   (channel-name :col-type text
                 :initarg :channel-name
                 :col-unique t
                 :initform ""
                 :accessor channel-name)
   (server :col-type text
           :initarg :server
           :initform "" :accessor server))
  (:documentation "Metadata for each channel the bot monitors. Not to be confused with
'BOT-IRC-CHANNEL. MURGH.")
  (:metaclass postmodern:dao-class)
  (:keys bot-channel-id))

(defun make-bot-channel-aux (channel-name server-name)
  (with-connection (psql-botdb-credentials *bot-config*)
    (handler-case
        (let ((bco (car (select-dao 'bot-channel (:= :channel-name channel-name)))))
          (if bco
              (values bco)
              (let ((bco (make-dao 'bot-channel
                                   :fetch-defaults t
                                   :channel-name channel-name
                                   :server server-name)))
                ;; (save-dao bco)
                (values bco))))
      (database-error (e)
        (log:debug "DB ERROR: ~A" e)
        ;; we need to save the dao instance when it's created, or we
        ;; don't get the id created in the database. Like so:
        (let ((bco (make-dao 'bot-channel
                             :fetch-defaults t
                             :channel-name channel-name
                             :server server-name)))
          (save-dao bco)
          (values bco))))))

(defun make-bot-channel (channel-name &optional (server-name nil))
  (log:debug "~2&Creating a channel object for channel: ~A~%" channel-name)
  (with-connection (psql-botdb-credentials *bot-config*)
    (let ((bco (make-bot-channel-aux channel-name server-name)))
      (log:debug "~2&TYPE OF BCO:[ ~S ] :: [ ~S ]~2%" (type-of bco) bco)
      
      (values bco))))

(defun get-bot-channel-for-name (name &optional (server-name ""))
  "Given a NAME, return the associated BOT-CHANNEL object."
  (with-connection (psql-botdb-credentials *bot-config*)
    (let ((c (make-bot-channel name server-name)))
      (values c))))

(defclass channel-user ()
  ((channel-id :col-type integer :col-references ((bot-channel bot-channel-id) :cascade) :initarg :channel-id :accessor channel-id)
   (user-id :col-type integer :col-references ((harlie-user harlie-user-id) :cascade) :initarg :user-id :accessor user-id)
   (ignored :col-type boolean :col-default nil :iniform nil :initarg :ignored :accessor ignored))
  (:documentation "A table to bridge between a channel and the users it contains.")
  (:metaclass postmodern:dao-class)
  (:keys channel-id user-id))

(defclass user-alias ()
  ((user-alias-id :col-type integer :col-references ((harlie-user harlie-user-id) :cascade)
                  :initarg :user-alias-id :accessor user-alias-id)
   (user-alias-name :col-type text :col-default nil :initform nil :initarg :alias-name :accessor user-alias-name))
  (:documentation "A table to link known user aliases to a given HALIE-USER")
  (:metaclass postmodern:dao-class)
  (:keys user-alias-id user-alias-name))

(defmethod print-object ((c/u channel-user) (out t))
  (print-unreadable-object (c/u out :type t)
    (format out "[|- CHANNEL-ID: ~D | USER-ID: ~A | IGNORED: ~A -|]"
            (channel-id c/u) (user-id c/u) (ignored c/u))))

(defclass harlie-user ()
  ((harlie-user-id :col-type serial
                   :reader harlie-user-id)
   
   (harlie-user-name :col-type text
                     :initarg :harlie-user-name
                     :col-unique t
                     :accessor harlie-user-name)
   
   (current-handle :col-type text
                   :initarg :current-handle
                   :col-unique t
                   :accessor current-handle)
   
   (prev-handle :col-type text
                :initarg :prev-handle
                :initform ""
                :accessor prev-handle)

   (email-address :col-type (or db-null text)
                  :initarg :email-address
                  :initform ""
                  :accessor email-address)

   (authenticated? :col-type boolean
                   :initarg :authenticated?
                   :initform nil
                   :accessor authenticated?)

   (first-seen :col-type timestamptz
               :initarg :first-seen
               :initform (local-time:now)
               :accessor first-seen)

   (last-seen :col-type timestamptz
              :initarg :last-seen
              :initform (local-time:now)
              :accessor last-seen)

   (harlie-user-memo :col-type text
                     :initarg :harlie-user-memo
                     :initform ""
                     :accessor harlie-user-memo))

  (:documentation "This table holds the user metadata for users being served by the bot.")
  (:metaclass postmodern:dao-class)
  ;; (:table-name harlie-users)
  (:keys harlie-user-id))

(defmacro with-handle-swap (user new-nick &body body)
  "Given a user USER and a new nickname NICKCHANGE, look up the user
object and swap the old handle with the new one.
   NEW-NICK -> the string representing the new user handle.
   OLD-NICK -> the string representing the old user handle.
   THIS-USER -> the user object holding various state."
  `(let* ((new-nick ,new-nick)
          (old-nick ,user)
          (this-user (get-user-for-handle ,user)))
     (log:info "THIS-USER: ~A OLD-NICK: ~A NEW-NICK: ~A" this-user old-nick new-nick)
     ,@body))

(defmacro with-channel-user (channel user &body body) ;;connection
  "Given a channel name and user nick as strings, bind the recorded
   HARLIE-USER, BOT-CHANNEL, and CHANNEL-USER that are associated with
   BOT-IRC-CHANNEL object mappings and return them as:
   CHAN -> BOT-IRC-CHANNEL object 
   THEUSERSTATE -> hash table for recording channel state in BOT-IRC-CHANNEL
   THIS-USER -> HALIE-USER object
   THIS-CHANNEL -> BOT-CHANNEL object (tag to key persistent state
   between runs in CHANNEL-USER). 
   CHANNEL/USER-MAP -> per channel state for each user
   in the database."
  `(let* ((chan (find-the-bot-state ,channel))
          (theuserstate (gethash ,user (ignore-sticky chan)))
          (this-user (get-user-for-handle ,user))
          (this-channel (get-bot-channel-for-name ,channel))
          (channel/user-map (get-channel-user-mapping this-channel this-user)))
     (log:debug "~2&<< [WITH-CHANNEL-USER] ~%THEUSERSTATE: ~A~% THIS-USER:~A /~% USER-NAME:~A~% THIS-CHANNEL:~A~% CHANNEL/USER-MAP:~A~%>>~2%" theuserstate this-user (harlie-user-name this-user) this-channel channel/user-map)
     ,@body))

(defgeneric get-channel-user-mapping (channel user)
  (:documentation "given a channel object and a user object, return a mapping between them in the database."))

(defmethod get-channel-user-mapping ((channel bot-channel) (user harlie-user))
  (with-connection (psql-botdb-credentials *bot-config*)
    (let ((c/u-map
            (select-dao 'channel-user (:and (:= 'channel-id (bot-channel-id channel))
                                            (:= 'user-id (harlie-user-id user))))))
      (if (or (and (listp c/u-map) (>= (length c/u-map) 1))
              (typep c/u-map 'channel-user))
          ;; this is ugly.
          (if (typep c/u-map 'channel-user)
              (values c/u-map)
              (values (first c/u-map)))
          (let* ((c/u-map (make-dao 'channel-user
                                    :channel-id (bot-channel-id channel)
                                    :user-id (harlie-user-id user))))
            (save-dao c/u-map)
            ;; (log:debug "~&[c/u-map] || ~A~%" (describe c/u-map))
            (values c/u-map))))))

(defun get-user-for-id (id)
  "Given an ID of type integer, return the associated channel user handle."
  (with-connection (psql-botdb-credentials *bot-config*)
    (let ((u (select-dao 'harlie-user  (:= 'harlie-user-id id))))
      (if (and (listp u) (>= (length u) 1))
          (first u)
          nil))))

(defun get-user-for-handle (handle &key channel)
  "Given a HANDLE, return the user from the database, or create one."
  (with-connection (psql-botdb-credentials *bot-config*)
    (let ((this-user (or (first (select-dao 'harlie-user (:= 'harlie-user-name handle)))
                         (first (select-dao 'harlie-user (:= 'current-handle handle)))
                         (make-dao 'harlie-user :harlie-user-name handle
                                                :current-handle handle ))))
      
      (log:debug "THIS IS THE VALUE OF THIS-USER: ~A" this-user)
      
      (if channel
          (log:debug "~&[HANDLE] : ~A [CHANNEL] : ~A~%" (current-handle this-user) channel)
          (log:debug "~&[HANDLE] : ~A" (current-handle this-user)))
      (assert (eq (type-of this-user) 'harlie-user))
      ;; #'select-dao can return a nested list. Account for it.
      (if (listp this-user)
          (upsert-dao (first this-user))
          (upsert-dao this-user))
      this-user)))

;; (defgeneric update-channel-user ((user channel-user) &key (:email :prev-handle :current-handle )))


(defun zero-bot-channels ()
  "Destroy and recreate the table to hold the channels the bot joins."
  (with-connection (psql-botdb-credentials *bot-config*)
    (drop-table "bot-channel" :if-exists t :cascade t)))

(defun make-bot-channels ()
  (with-connection (psql-botdb-credentials *bot-config*)
    (query (dao-table-definition 'bot-channel))))

(defun zero-channel-users ()
  "destroy and recreate the table to hold a channel's persistent users."
  (with-connection (psql-botdb-credentials *bot-config*)
    (drop-table 'channel-user :if-exists t :cascade t)))

(defun make-channel-users ()
  (with-connection (psql-botdb-credentials *bot-config*)
    (query (dao-table-definition 'channel-user))))

(defun zero-harlie-users ()
  "destroy the table that holds the users known to the bot."
  (with-connection (psql-botdb-credentials *bot-config*)
    (drop-table "harlie-user" :if-exists t :cascade t)))

(defun make-harlie-users ()
  (with-connection (psql-botdb-credentials *bot-config*)
    (query (dao-table-definition 'harlie-user))))

(defun zero-users ()
  "Let's just... start all over again."
  (setf *users* (make-hash-table :test 'equalp :synchronized t))
  (zero-harlie-users)
  (zero-bot-channels)
  (zero-channel-users))

(defun make-users ()
  (make-bot-channels)
  (make-harlie-users)
  (make-channel-users))


;; (defun make-an-harlie-user (nick-message)
;;   "given an irc user handle in nick-message, create an instance of the
;; 'harlie-user dao class."
;;   (log:debug "~2&KLEEVO! [ ~A ]~2%" (describe nick-message))
;;   (let ((this-user
;;           (make-dao 'harlie-user
;;                     :harlie-user nick-message
;;                     :current-handle nick-message
;;                     :prev-handle nil
;;                     :authenticated nil
;;                     :fetch-defaults t)))
;;     (upsert-dao this-user)))

;; (defun make-a-new-harlie-user (nick)
;;   (with-connection (psql-botdb-credentials *bot-config*)
;;     (let ((uobject (get-user-for-handle nick)))
;;       (if uobject
;;           (update-dao uobject)
;;           (make-an-harlie-user nick)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; users.lisp ends here
