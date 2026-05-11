;;;; phrases.lisp — Bot phrase storage and voting
;;
;; Every Markov-chain utterance is persisted so that channel members
;; can vote for the funniest ones.  The top-voted phrases are
;; available via the !top command and a /phrases web endpoint.

(in-package #:harlie)

;;; ---- Database operations ------------------------------------------------

(defun db-store-phrase (context-id channel trigger-text phrase-text)
  "Persist a bot-generated phrase.  Returns the new phrase_id."
  (with-connection (db-credentials *bot-config*)
    (query (:insert-into 'phrases :set
            'context-id  context-id
            'channel     channel
            'trigger-text trigger-text
            'phrase-text  phrase-text)
           :none)
    (query (:select (:raw "currval('phrases_phrase_id_seq')")) :single)))

(defun db-vote-phrase (phrase-id voter)
  "Cast a vote for PHRASE-ID by VOTER.
Returns :ok, :already-voted, or :not-found."
  (with-connection (db-credentials *bot-config*)
    (let ((exists (query (:select 'phrase-id :from 'phrases
                          :where (:= 'phrase-id phrase-id))
                         :single)))
      (cond
        ((null exists) :not-found)
        (t (handler-case
               (progn
                 (query (:insert-into 'phrase-votes :set
                         'phrase-id phrase-id
                         'voter     voter)
                        :none)
                 :ok)
             ;; unique violation (PG 23505) means already voted
             (database-error (e)
               (if (string= "23505" (cl-postgres::database-error-code e))
                   :already-voted
                   (error e)))))))))

(defun db-get-phrase (phrase-id)
  "Retrieve a phrase by ID.  Returns (phrase-id channel trigger phrase vote-count) or NIL."
  (with-connection (db-credentials *bot-config*)
    (first
     (query (:select 'p.phrase-id 'p.channel 'p.trigger-text 'p.phrase-text
                     (:raw "COALESCE(v.votes, 0) AS vote_count")
             :from (:as 'phrases 'p)
             :left-join (:as (:select 'phrase-id (:as (:count '*) 'votes)
                              :from 'phrase-votes
                              :group-by 'phrase-id)
                             'v)
             :on (:= 'p.phrase-id 'v.phrase-id)
             :where (:= 'p.phrase-id phrase-id))
            :rows))))

(defun db-top-phrases (channel &optional (limit 10))
  "Return the top voted phrases for CHANNEL.
Each row is (phrase-id trigger-text phrase-text vote-count)."
  (with-connection (db-credentials *bot-config*)
    (query (:limit
            (:order-by
             (:select 'p.phrase-id 'p.trigger-text 'p.phrase-text
                      (:raw "COALESCE(v.votes, 0) AS vote_count")
              :from (:as 'phrases 'p)
              :left-join (:as (:select 'phrase-id (:as (:count '*) 'votes)
                               :from 'phrase-votes
                               :group-by 'phrase-id)
                              'v)
              :on (:= 'p.phrase-id 'v.phrase-id)
              :where (:= 'p.channel channel))
             (:desc (:raw "vote_count")) (:desc 'p.phrase-id))
            limit)
           :rows)))

(defun db-top-phrases-for-web (context-id &optional (limit 50))
  "Return the top voted phrases for a context (used by the web endpoint).
Each row is (phrase-id channel trigger-text phrase-text vote-count created-at)."
  (with-connection (db-credentials *bot-config*)
    (query (:limit
            (:order-by
             (:select 'p.phrase-id 'p.channel 'p.trigger-text 'p.phrase-text
                      (:raw "COALESCE(v.votes, 0) AS vote_count")
                      'p.created-at
              :from (:as 'phrases 'p)
              :left-join (:as (:select 'phrase-id (:as (:count '*) 'votes)
                               :from 'phrase-votes
                               :group-by 'phrase-id)
                              'v)
              :on (:= 'p.phrase-id 'v.phrase-id)
              :where (:= 'p.context-id context-id))
             (:desc (:raw "vote_count")) (:desc 'p.phrase-id))
            limit)
           :rows)))

(defun db-phrase-vote-count (phrase-id)
  "Return the number of votes for PHRASE-ID."
  (with-connection (db-credentials *bot-config*)
    (or (query (:select (:count '*)
               :from 'phrase-votes
               :where (:= 'phrase-id phrase-id))
              :single)
        0)))
