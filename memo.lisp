;;;; memo.lisp — Leave-a-message / !tell system
;;
;; In-memory memo store with optional DB persistence.
;; Memos are stored per-recipient (case-insensitive nick) and delivered
;; when the recipient next joins a channel or speaks.

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

;;; Core operations

(defun store-memo (sender recipient channel message)
  "Store a memo for RECIPIENT from SENDER in CHANNEL.
Returns the newly created pending-memo, or NIL if inputs are invalid."
  (when (and sender recipient channel message
             (> (length (string-trim " " message)) 0)
             (not (string-equal sender recipient)))
    (let ((memo (make-pending-memo :sender sender
                                    :recipient recipient
                                    :channel channel
                                    :message (string-trim " " message))))
      (push memo (gethash recipient *pending-memos*))
      memo)))

(defun pending-memos-for (recipient)
  "Return a list of undelivered memos for RECIPIENT (case-insensitive).
Most recent first."
  (gethash recipient *pending-memos*))

(defun pending-memo-count (recipient)
  "Return the number of pending memos for RECIPIENT."
  (length (gethash recipient *pending-memos*)))

(defun has-pending-memos-p (recipient)
  "Return T if RECIPIENT has any pending memos."
  (not (null (gethash recipient *pending-memos*))))

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
    (format nil "~A: ~A left a message for you (~A ago): ~A"
            (pending-memo-recipient memo)
            (pending-memo-sender memo)
            age
            (pending-memo-message memo))))

(defun deliver-memos-for (conn recipient &key (channel nil))
  "Deliver all pending memos for RECIPIENT via CONN.
If CHANNEL is given, deliver there; otherwise deliver to each memo's original channel.
Returns the number of memos delivered."
  (let ((memos (pending-memos-for recipient))
        (count 0))
    (dolist (memo (reverse memos))  ; deliver oldest first
      (let ((target (or channel (pending-memo-channel memo)))
            (text (format-memo-delivery memo)))
        (handler-case
            (progn
              (privmsg conn target text)
              (incf count))
          (error (e)
            (log:warn "~&[MEMO] Error delivering memo to ~A: ~A" recipient e)))))
    ;; Remove delivered memos
    (when (> count 0)
      (remhash recipient *pending-memos*))
    count))

;;; Query helpers (for !memos command)

(defun list-memo-senders (recipient)
  "Return a list of who has left memos for RECIPIENT."
  (mapcar #'pending-memo-sender (pending-memos-for recipient)))
