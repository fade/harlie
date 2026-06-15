;;;; polls.lisp - Timed, multiple-choice channel polls
;;
;; A poll is defined privately (in a query window), published to a
;; channel, and voted on by channel members - one vote per nick.  Each
;; poll carries an expiry; a periodic sweeper closes any poll whose timer
;; has elapsed and announces the results in its channel.  Because the
;; expiry lives in the database rather than in a sleeping thread, polls
;; survive bot restarts.
;;
;; See database/add-polls-tables.sql for the schema and the !poll plugin
;; in plugins.lisp for the user-facing command surface.

(in-package #:harlie)

;;; ---- Argument parsing ---------------------------------------------------

(defun split-quoted (string)
  "Split STRING on whitespace, treating double-quoted spans as a single
token.  Surrounding double quotes are removed.  Empty tokens are dropped."
  (let ((tokens '())
        (current (make-string-output-stream))
        (in-quote nil)
        (have-token nil))
    (flet ((flush ()
             (when have-token
               (push (get-output-stream-string current) tokens)
               (setf have-token nil))))
      (loop for ch across string do
        (cond
          ((char= ch #\")
           (setf in-quote (not in-quote)
                 have-token t))
          ((and (not in-quote) (member ch '(#\Space #\Tab)))
           (flush))
          (t
           (write-char ch current)
           (setf have-token t))))
      (flush))
    (remove "" (nreverse tokens) :test #'string=)))

;;; ---- Database operations ------------------------------------------------

(defun db-create-poll (channel question creator seconds options)
  "Create a poll in CHANNEL with QUESTION and OPTIONS (a list of strings),
expiring SECONDS from now.  Returns the new poll_id."
  (with-connection (db-credentials *bot-config*)
    (query (:insert-into 'polls :set
            'channel    channel
            'question   question
            'creator    creator
            'expires-at (:raw (format nil "now() + interval '~D seconds'" seconds)))
           :none)
    (let ((poll-id (query (:select (:raw "currval('polls_poll_id_seq')")) :single)))
      (loop for opt in options
            for n from 1
            do (query (:insert-into 'poll-options :set
                       'poll-id     poll-id
                       'option-num  n
                       'option-text opt)
                      :none))
      poll-id)))

(defun db-get-poll (poll-id)
  "Return (poll-id channel question creator expires-at published closed
announced) for POLL-ID, or NIL if it does not exist."
  (with-connection (db-credentials *bot-config*)
    (first
     (query (:select 'poll-id 'channel 'question 'creator 'expires-at
                     'published 'closed 'announced
             :from 'polls :where (:= 'poll-id poll-id))
            :rows))))

(defun db-poll-options (poll-id)
  "Return the options for POLL-ID as a list of (option-num option-text)."
  (with-connection (db-credentials *bot-config*)
    (query (:order-by
            (:select 'option-num 'option-text :from 'poll-options
             :where (:= 'poll-id poll-id))
            'option-num)
           :rows)))

(defun db-poll-results (poll-id)
  "Return a list of (option-num option-text votes) for POLL-ID, ordered by
option-num."
  (with-connection (db-credentials *bot-config*)
    (query (:order-by
            (:select 'o.option-num 'o.option-text
                     (:raw "COALESCE(v.votes, 0) AS votes")
             :from (:as 'poll-options 'o)
             :left-join (:as (:select 'option-num (:as (:count '*) 'votes)
                              :from 'poll-votes
                              :where (:= 'poll-id poll-id)
                              :group-by 'option-num)
                             'v)
             :on (:= 'o.option-num 'v.option-num)
             :where (:= 'o.poll-id poll-id))
            'o.option-num)
           :rows)))

(defun db-publish-poll (poll-id)
  "Mark POLL-ID as published."
  (with-connection (db-credentials *bot-config*)
    (query (:update 'polls :set 'published t :where (:= 'poll-id poll-id))
           :none)))

(defun db-vote-poll (poll-id voter option-num)
  "Record VOTER's choice of OPTION-NUM in POLL-ID.
Returns :ok, :already-voted, :not-found, :not-open, or :invalid-option."
  (with-connection (db-credentials *bot-config*)
    (let ((poll (query (:select 'published 'closed
                                (:raw "(expires_at <= now()) AS expired")
                        :from 'polls :where (:= 'poll-id poll-id))
                       :row)))
      (if (null poll)
          :not-found
          (destructuring-bind (published closed expired) poll
            (cond
              ((or (not published) closed expired) :not-open)
              ((null (query (:select 'option-num :from 'poll-options
                             :where (:and (:= 'poll-id poll-id)
                                          (:= 'option-num option-num)))
                            :single))
               :invalid-option)
              (t (handler-case
                     (progn
                       (query (:insert-into 'poll-votes :set
                               'poll-id    poll-id
                               'voter      voter
                               'option-num option-num)
                              :none)
                       :ok)
                   ;; unique violation (PG 23505) means already voted
                   (database-error (e)
                     (if (string= "23505" (cl-postgres::database-error-code e))
                         :already-voted
                         (error e)))))))))))

(defun db-close-poll (poll-id)
  "Mark POLL-ID closed (no further votes accepted)."
  (with-connection (db-credentials *bot-config*)
    (query (:update 'polls :set 'closed t :where (:= 'poll-id poll-id))
           :none)))

(defun db-mark-announced (poll-id)
  "Mark POLL-ID as closed and its results announced."
  (with-connection (db-credentials *bot-config*)
    (query (:update 'polls :set 'closed t 'announced t
            :where (:= 'poll-id poll-id))
           :none)))

(defun db-pending-poll-announcements ()
  "Return the poll-ids of published polls that have expired or been closed
but not yet announced."
  (with-connection (db-credentials *bot-config*)
    (query (:select 'poll-id :from 'polls
            :where (:and 'published
                         (:not 'announced)
                         (:or 'closed (:raw "expires_at <= now()"))))
           :column)))

(defun db-open-polls (channel)
  "Return (poll-id question) for published, still-open polls in CHANNEL."
  (with-connection (db-credentials *bot-config*)
    (query (:order-by
            (:select 'poll-id 'question :from 'polls
             :where (:and (:= 'channel channel)
                          'published
                          (:not 'closed)
                          (:raw "expires_at > now()")))
            'poll-id)
           :rows)))

(defun db-polls-by-creator (creator &optional (limit 10))
  "Return (poll-id channel question published closed) for the most recent
polls created by CREATOR."
  (with-connection (db-credentials *bot-config*)
    (query (:limit
            (:order-by
             (:select 'poll-id 'channel 'question 'published 'closed
              :from 'polls :where (:= 'creator creator))
             (:desc 'poll-id))
            limit)
           :rows)))

(defun db-polls-for-web (channel &optional (limit 25))
  "Return recent published polls for CHANNEL for the web view, newest first.
Each row is (poll-id question creator created-at expires-at closed expired)."
  (with-connection (db-credentials *bot-config*)
    (query (:limit
            (:order-by
             (:select 'poll-id 'question 'creator 'created-at 'expires-at 'closed
                      (:raw "(expires_at <= now()) AS expired")
              :from 'polls
              :where (:and (:= 'channel channel) 'published))
             (:desc 'poll-id))
            limit)
           :rows)))

;;; ---- Formatting ---------------------------------------------------------

(defun poll-bar (votes maxv &optional (width 10))
  "Return a proportional bar of block characters for VOTES relative to MAXV."
  (if (or (zerop maxv) (zerop votes))
      ""
      (make-string (max 1 (round (* width (/ votes maxv))))
                   :initial-element #\FULL_BLOCK)))

(defun poll-winner (results)
  "Return a human-readable string naming the winning option(s) in RESULTS
\(a list of (option-num option-text votes)), noting ties."
  (let ((maxv (reduce #'max results :key #'third :initial-value 0)))
    (if (zerop maxv)
        "no votes cast"
        (let ((winners (remove maxv results :key #'third :test-not #'=)))
          (format nil "~{~A~^, ~}~:[~; (tie)~]"
                  (mapcar #'second winners)
                  (> (length winners) 1))))))

(defun format-poll-deadline (expires-at)
  "Describe how long until EXPIRES-AT (a local-time timestamp)."
  (let ((secs (- (timestamp-to-universal expires-at) (get-universal-time))))
    (if (plusp secs)
        (format nil "in ~A" (format-duration secs))
        "soon")))

(defun format-poll-timestamp (timestamp)
  "Format TIMESTAMP (a local-time timestamp) as a compact
\"YYYY-MM-DD HH:MM\" string, or \"unknown\" when it is missing."
  (if timestamp
      (local-time:format-timestring
       nil timestamp
       :format '((:year 4) #\- (:month 2) #\- (:day 2) #\Space
                 (:hour 2) #\: (:min 2)))
      "unknown"))

(defparameter *poll-options-line-max* 400
  "Maximum characters of an options-line's content before it is split across
multiple PRIVMSGs.  Kept well under IRC's ~512-byte line limit to leave room
for the channel target and the \"poll:: \" prefix the publisher prepends.")

(defun pack-poll-options (options &key (max-width *poll-options-line-max*))
  "Format OPTIONS (a list of (num text)) as \"N) text\" entries packed,
greedily, into as few indented lines as possible without exceeding MAX-WIDTH
characters per line.  Normally everything fits on one line; only genuinely
long option labels spill onto further lines.  Returns a list of strings."
  (let ((entries (loop for (num text) in options
                       collect (format nil "~D) ~A" num text)))
        (lines '())
        (current nil))
    (dolist (entry entries)
      (let ((candidate (if current (format nil "~A   ~A" current entry) entry)))
        (if (and current (> (length candidate) max-width))
            (progn (push current lines)
                   (setf current entry))
            (setf current candidate))))
    (when current (push current lines))
    (mapcar (lambda (line) (format nil "  ~A" line)) (nreverse lines))))

(defun format-poll-announcement (poll-id)
  "Build the channel announcement published for POLL-ID as a compact layout:
a header, the options (normally a single line), and a vote / closes line.
Returns a list of strings, or NIL if the poll is missing.

IRC has no multi-line message, so each element is sent as its own PRIVMSG;
keeping the options together on one line lets a multi-option poll be read at
a glance instead of scattered across several messages.  PACK-POLL-OPTIONS
splits the options onto additional lines should they exceed a safe length."
  (let ((poll (db-get-poll poll-id))
        (options (db-poll-options poll-id)))
    (when poll
      (destructuring-bind (pid channel question creator expires-at
                           published closed announced) poll
        (declare (ignore channel creator published closed announced))
        (append
         (list (format nil "📊  Poll #~D: ~A" pid question))
         (pack-poll-options options)
         (list (format nil "  Vote: !poll vote ~D <n>  ·  closes ~A"
                        pid (format-poll-deadline expires-at))))))))

(defun format-poll-results (poll-id &key closed)
  "Build a multi-line results report for POLL-ID.  When CLOSED is true the
report is framed as a final tally and names the winner.  Returns a list of
strings, or NIL if the poll is missing."
  (let ((poll (db-get-poll poll-id))
        (results (db-poll-results poll-id)))
    (when poll
      (destructuring-bind (pid channel question creator expires-at
                           published closed-flag announced) poll
        (declare (ignore channel creator expires-at published closed-flag announced))
        (let* ((total (reduce #'+ results :key #'third :initial-value 0))
               (maxv  (reduce #'max results :key #'third :initial-value 0)))
          (append
           (list (format nil "~A  Poll #~D: ~A (~D vote~:P)"
                         (if closed "⏱" "📊") pid question total))
           (loop for (num text votes) in results
                 collect (format nil "  ~D) ~A  ~A ~D"
                                 num text (poll-bar votes maxv) votes))
           (when closed
             (list (format nil "Winner: ~A" (poll-winner results))))))))))

;;; ---- Expiry sweeper -----------------------------------------------------

(defvar *poll-sweeper-task* nil
  "The periodic task that closes and announces expired polls.")

(defparameter *poll-sweep-interval* 30
  "Seconds between sweeps for expired polls.")

(defun sweep-expired-polls ()
  "Close and announce any published poll that has expired or been closed
but not yet announced.  Safe to call repeatedly; errors are logged, not
signalled, so one bad poll cannot stall the sweeper."
  (handler-case
      (dolist (poll-id (db-pending-poll-announcements))
        (handler-case
            (let ((poll (db-get-poll poll-id)))
              (when poll
                (let ((channel (second poll)))
                  ;; Mark first so a send failure cannot cause repeated spam.
                  (db-mark-announced poll-id)
                  (dolist (line (format-poll-results poll-id :closed t))
                    (say (format nil "poll:: ~A" line) :channel channel)))))
          (error (e)
            (log:warn "~&[POLL] Error announcing poll #~D: ~A" poll-id e))))
    (error (e)
      (log:warn "~&[POLL] Sweeper error: ~A" e))))

(defun start-poll-sweeper ()
  "Start the periodic poll-expiry sweeper unless it is already running."
  (unless *poll-sweeper-task*
    (setf *poll-sweeper-task*
          (make-task #'sweep-expired-polls *poll-sweep-interval* "poll-sweeper"))
    (log:info "~&[POLL] Started poll-expiry sweeper (every ~Ds)."
              *poll-sweep-interval*)))

(defun stop-poll-sweeper ()
  "Stop the poll-expiry sweeper if it is running."
  (when *poll-sweeper-task*
    (stop-task *poll-sweeper-task*)
    (setf *poll-sweeper-task* nil)))
