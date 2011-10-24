;;;; chainer.lisp

(in-package :harlie)

(defun count-phrases ()
  "Return the number of entries in the words table."
  (with-connection *chain-db*
    (query (:select (:raw "count(*)") :from 'words) :single)))

(defun fetch-start (rownum)
  "Select an entry from the words table by row number, and return word3."
  (query (:select 'word3
	  :from 'words
	  :where (:and (:= 'row-num rownum)
		       (:= 'word1 (string #\Newline))
		       (:= 'word2 (string #\Newline)))) :single))

(defun random-start ()
  "Find a random starting point for a chain.  Return the word which starts
the chain, and also the number of trials before finding it."
  (let* ((numrows (query (:select (:raw "max(row_num)") :from 'words) :single)))
    (do* ((rownum (random (1+ numrows)) (random (1+ numrows)))
	  (r (fetch-start rownum) (fetch-start rownum))
	  (n 1 (1+ n)))
	 (r (values r n)))))

(defun chain-next (word1 word2)
  "Retrieve a random word to go next in the chain."
  (query (:limit
	  (:order-by
	   (:select 'word3
	    :from 'words
	    :where (:and (:= (:raw "upper(word1)") (string-upcase word1))
			 (:= (:raw "upper(word2)") (string-upcase word2))))
	   (:raw "random()"))
	  1) :single))

(defun chain (&optional w1 w2 )
  "Generate a full random chain.  If desired, you can specify the first
word or two of the chain.  Returns a list of strings."
  (with-connection *chain-db*
    ;; If w1 and/or w2 are provided, use them.  Otherwise use sentinel values.
    (do* ((word1 (if (and w1 w2)
		     w1
		     (string #\Newline))
		 word2)
	  (word2 (cond ((and w1 w2) w2)
		       (w1 w1)
		       (t (string #\Newline)))
		 word3)
	  ;; If no w1 or w2 is specified, then pick a random starting point.
	  ;; Otherwise, start chaining.
	  (word3 (if (not (or w1 w2))
		     (random-start)
		     (chain-next word1 word2))
		 (chain-next word1 word2))
	  (utterance (remove-if
		      (lambda (s)
			(equal s (string #\Newline)))
		      (list word1 word2 word3)) 
		     (if (equal word3 (string #\Newline))
			 utterance
			 (append utterance (list word3)))))
	 ((or (not word3) (equal word3 (string #\Newline))) utterance))))
