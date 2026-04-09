;;;; memo.lisp — Leave-a-message / !tell system
;;
;; Memos are stored per-recipient (case-insensitive nick) and delivered
;; when the recipient next joins a channel or speaks.
;;
;; Persistence: memos are written through to PostgreSQL (pending_memos
;; table) and loaded back into the in-memory cache on startup.  Run
;; database/add-pending-memos-table.sql to create the table.

(in-package #:harlie)

;;; Memo structure

(defstruct (pending-memo (:print-function print-pending-memo))
  "A message left for an offline user."
  sender
  recipient
  channel
  message
  (created-at (get-universal-time))
  (delivered-p nil))

(defun print-pending-memo (memo stream depth)
  (declare (ignore depth))
  (print-unreadable-object (memo stream :type t)
    (format stream "~A→~A ~S" (pending-memo-sender memo)
            (pending-memo-recipient memo)
            (pending-memo-message memo))))

;;; In-memory store

(defvar *pending-memos* (make-hash-table :test 'equalp)
  "Hash of recipient-nick (string, case-insensitive) → list of pending-memo structs.
Only undelivered memos are kept; delivered ones are removed immediately.")

(defun clear-all-memos ()
  "Clear the in-memory memo store. Useful for tests."
  (clrhash *pending-memos*))

;;; Database persistence

(defun db-store-memo (memo)
  "Persist a pending-memo to the database. Returns the new memo_id."
  (handler-case
      (with-connection (db-credentials *bot-config*)
        (query (:insert-into 'pending-memos :set
                'sender     (pending-memo-sender memo)
                'recipient  (pending-memo-recipient memo)
                'channel    (pending-memo-channel memo)
                'message    (pending-memo-message memo)
                'created-at (pending-memo-created-at memo))
               :none)
        (query (:select (:raw "currval('pending_memos_memo_id_seq')")) :single))
    (error (e)
      (log:warn "~&[MEMO] DB store failed: ~A" e)
      nil)))

(defun db-delete-memos-for (recipient)
  "Delete all pending memos for RECIPIENT from the database."
  (handler-case
      (with-connection (db-credentials *bot-config*)
        (query (:delete-from 'pending-memos
                :where (:= (:lower 'recipient) (:lower recipient)))
               :none))
    (error (e)
      (log:warn "~&[MEMO] DB delete failed for ~A: ~A" recipient e))))

(defun db-delete-memos-for-channel (recipient channel)
  "Delete pending memos for RECIPIENT that were left in CHANNEL."
  (handler-case
      (with-connection (db-credentials *bot-config*)
        (query (:delete-from 'pending-memos
                :where (:and (:= (:lower 'recipient) (:lower recipient))
                             (:= (:lower 'channel) (:lower channel))))
               :none))
    (error (e)
      (log:warn "~&[MEMO] DB delete failed for ~A/~A: ~A" recipient channel e))))

(defun load-memos-from-db ()
  "Load all pending memos from the database into the in-memory cache.
  Called once at bot startup to restore memos that survived a restart."
  (handler-case
      (with-connection (db-credentials *bot-config*)
        (let ((rows (query (:order-by
                            (:select 'sender 'recipient 'channel
                                     'message 'created-at
                             :from 'pending-memos)
                            'created-at)
                           :rows))
              (count 0))
          (clrhash *pending-memos*)
          (dolist (row rows)
            (let ((memo (make-pending-memo
                         :sender     (first row)
                         :recipient  (second row)
                         :channel    (third row)
                         :message    (fourth row)
                         :created-at (fifth row))))
              (push memo (gethash (second row) *pending-memos*))
              (incf count)))
          (when (> count 0)
            (log:info "~&[MEMO] Loaded ~D pending memo~:P from database." count))
          count))
    (error (e)
      (log:warn "~&[MEMO] Failed to load memos from DB: ~A" e)
      0)))

;;; Core operations

(defun store-memo (sender recipient channel message)
  "Store a memo for RECIPIENT from SENDER in CHANNEL.
Persists to the database and caches in memory.
Returns the newly created pending-memo, or NIL if inputs are invalid."
  (when (and sender recipient channel message
             (> (length (string-trim " " message)) 0)
             (not (string-equal sender recipient)))
    (let ((memo (make-pending-memo :sender sender
                                    :recipient recipient
                                    :channel channel
                                    :message (string-trim " " message))))
      (push memo (gethash recipient *pending-memos*))
      (db-store-memo memo)
      memo)))

(defun pending-memos-for (recipient)
  "Return a list of undelivered memos for RECIPIENT (case-insensitive).
Most recent first."
  (gethash recipient *pending-memos*))

(defun pending-memos-for-channel (recipient channel)
  "Return only the pending memos for RECIPIENT that were left in CHANNEL."
  (remove-if-not (lambda (memo)
                   (string-equal (pending-memo-channel memo) channel))
                 (gethash recipient *pending-memos*)))

(defun pending-memo-count (recipient)
  "Return the number of pending memos for RECIPIENT."
  (length (gethash recipient *pending-memos*)))

(defun has-pending-memos-p (recipient)
  "Return T if RECIPIENT has any pending memos."
  (not (null (gethash recipient *pending-memos*))))

(defun has-pending-memos-in-channel-p (recipient channel)
  "Return T if RECIPIENT has any pending memos that were left in CHANNEL."
  (not (null (pending-memos-for-channel recipient channel))))

;;; Age formatting

(defun format-memo-age (seconds)
  "Format a duration in SECONDS into a human-readable string."
  (cond ((< seconds 60) "just now")
        ((< seconds 3600)
         (let ((mins (floor seconds 60)))
           (format nil "~D minute~:P" mins)))
        ((< seconds 86400)
         (let ((hours (floor seconds 3600)))
           (format nil "~D hour~:P" hours)))
        (t
         (let ((days (floor seconds 86400)))
           (format nil "~D day~:P" days)))))

;;; Delivery

(defun format-memo-delivery (memo)
  "Format a single memo for delivery as an IRC message."
  (let ((age (format-memo-age (- (get-universal-time)
                                 (pending-memo-created-at memo)))))
    (format nil "✉  ~A: ~A left a message for you (~A ago): ~A"
            (pending-memo-recipient memo)
            (pending-memo-sender memo)
            age
            (pending-memo-message memo))))

(defun deliver-memos-for (conn recipient &key (channel nil))
  "Deliver pending memos for RECIPIENT via CONN.
When CHANNEL is given, only deliver memos that were left in that channel
and deliver them there.  Memos left in other channels remain pending.
Returns the number of memos delivered."
  (let* ((all-memos (pending-memos-for recipient))
         (to-deliver (if channel
                        (remove-if-not
                         (lambda (m) (string-equal (pending-memo-channel m) channel))
                         all-memos)
                        all-memos))
         (count 0))
    (dolist (memo (reverse to-deliver))  ; deliver oldest first
      (let ((target (or channel (pending-memo-channel memo)))
            (text (format-memo-delivery memo)))
        (handler-case
            (progn
              (privmsg conn target text)
              (incf count))
          (error (e)
            (log:warn "~&[MEMO] Error delivering memo to ~A: ~A" recipient e)))))
    ;; Remove only the delivered memos
    (when (> count 0)
      (if channel
          (let ((remaining (remove-if
                            (lambda (m) (string-equal (pending-memo-channel m) channel))
                            all-memos)))
            (if remaining
                (setf (gethash recipient *pending-memos*) remaining)
                (remhash recipient *pending-memos*))
            (db-delete-memos-for-channel recipient channel))
          (progn
            (remhash recipient *pending-memos*)
            (db-delete-memos-for recipient))))
    count))

;;; Query helpers (for !memos command)

(defun list-memo-senders (recipient)
  "Return a list of who has left memos for RECIPIENT."
  (mapcar #'pending-memo-sender (pending-memos-for recipient)))
