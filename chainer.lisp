;;;; chainer.lisp

(in-package :harlie)

(defparameter *sentinel* (string #\Newline))

(defclass words ()
  ((word1 :col-type string :initarg :word1 :accessor word1)
   (word2 :col-type string :initarg :word2 :accessor word2)
   (word3 :col-type string :initarg :word3 :accessor word3)
   (incidence :col-type integer :initform 1 :accessor incidence)
   (row-num :col-type integer :accessor row-num))
  (:metaclass dao-class)
  (:keys row-num))

(defun chain-in (context toklist)
  (unless (< (length toklist) 3)
    (with-connection (chain-write-credentials context)
      (do* ((word1 *sentinel* word2)
	    (word2 *sentinel* word3)
	    (word3 (pop toklist) (pop toklist))
	    (context-id (chain-write-context-id context)))
	   ((and (not word3) (not word2))  nil)
	(insert-dao (make-instance 'words :word1 word1
					  :word2 (if word2 word2 *sentinel*)
					  :word3 (if word3 word3 *sentinel*)
					  :context-id context-id))))))

(defun count-phrases (context)
  "Return the number of entries in the words table."
  (with-connection (chain-read-credentials context)
    (query (:select
	    (:raw "count(*)")
	    :from 'words
	    :where (:= 'context-id
		       (chain-read-context-id context)))
	   :single)))

(defun fetch-start (context rownum)
  "Select a chain-starting entry from the words table by row number, and return word3."
  (query (:select 'word3
	  :from 'words
	  :where (:and (:= 'row-num rownum)
		       (:= 'word1 *sentinel*)
		       (:= 'word2 *sentinel*)
		       (:= 'context-id (chain-read-context-id context))))
	 :single))

(defun random-start (context)
  "Find a random starting point for a chain.  Return the word which starts
the chain, and also the number of trials before finding it."
  (let ((numrows (query (:select (:raw "max(row_num)") :from 'words) :single)))
    (do* ((rownum (random (1+ numrows)) (random (1+ numrows)))
	  (r (fetch-start context rownum) (fetch-start context rownum))
	  (n 1 (1+ n)))
	 (r (values r n)))))

(defun fetch-row (rownum)
  "Select an entry from the words table by row number, and return word2."
  (query (:select 'word2
	  :from 'words
	  :where (:and (:= 'row-num rownum)
		       (:!= 'word2 *sentinel*))) :single))

(defun random-numbers (how-many how-large)
  "Returns how-many random integers in the range [1, how-large]."
  (loop for i from 1 to how-many collecting (1+ (random how-large))))

(defun make-random-words-query (context-id n range)
  (let* ((query-head '(:select 'word2 :from 'words :where))
	 (indices (random-numbers n range))
	 (clauses (loop for x in indices collecting (list ':= 'row-num x)))
	 )
    (sql-compile
     (append query-head (list (list ':and
				    (cons ':or clauses)
				    `(:= 'context-id ,context-id)))))))

(defun acceptable-word-p (w)
  (and (not (string= (string #\Newline) w))
       (>= (length w) 4) ;; Redundant, but let's leave it in for example's sake
       (scan "^[a-zA-Z]*$" w)))

(defparameter *words-safe* nil)

(defun random-words (context n &optional (wordp #'identity))
  "Return n random words from the chaining db, filtering with predicate wordp."
  (let ((readcreds (chain-read-credentials context))
	(readconid (chain-read-context-id context)))
    (with-connection readcreds
      (unless (member readconid *words-safe*)
	(if (= 0 (query (:select (:raw "count(*)") :from 'words :where (:= 'context-id readconid)) :single))
	    (progn
	      (format t "Your words database is empty.  There are no trigger words.")
	      (return-from random-words nil))))
      (let ((table-length (query
			   (:select
			    (:raw "max(row_num)")
			    :from 'words) :single))
	    (context-id readconid))
	(loop appending
	      (remove-if-not wordp
			     (mapcar #'car
				     (query (make-random-words-query
					     context-id 100 table-length))))
		into rwords
	      until (>= (length rwords) n)
	      finally (return (subseq rwords 0 n)))))    )
  )

(defun chain-next (context word1 word2)
  "Retrieve a random word to go next in the chain."
  (query (:limit
	  (:order-by
	   (:select 'word3
	    :from 'words
	    :where (:and (:= (:raw "upper(word1)") (string-upcase word1))
			 (:= (:raw "upper(word2)") (string-upcase word2))
			 (:= 'context-id (chain-read-context-id context))))
	   (:raw "random()"))
	  1) :single))

(defun argshift (filler &optional w1 w2)
  "Shift arguments to fill in from the right."
  (let ((stack (list nil nil))
	(queue (list w1 w2 nil)))
    (do* ((arg (pop queue) (pop queue)))
	 ((not queue) (substitute filler nil (reverse (subseq stack 0 2))))
      (when arg (push arg stack)))))

(defun chain (context &optional w1 w2)
  "Generate a full random chain.  If desired, you can specify the first
word or two of the chain.  Returns a list of strings."
  (with-connection (chain-read-credentials context)
    ;; If w1 and/or w2 are provided, use them.  Otherwise use sentinel values.
    (destructuring-bind (a b) (argshift *sentinel* w1 w2)
      (do* ((word1 a word2)
	    (word2 b word3)
	    ;; If no w1 or w2 is specified, then pick a random starting point.
	    ;; Otherwise, start chaining.
	    (word3 (if (not (or w1 w2))
		       (random-start context)
		       (chain-next context word1 word2))
		   (chain-next context word1 word2))
	    (utterance (list word1 word2 word3) 
		       (append utterance (list word3))))
	   ((or (not word3) (equal word3 *sentinel*)) (remove *sentinel* (butlast utterance) :test #'equal))))))

(defun accept-n (l n)
  "Test to see whether an n-syllable sequence appears at the start of l.
l is a list of conses from make-syllable-sums.  Return the n-syllable
sequence, or nil if not found."
  (when (equal l nil) (return-from accept-n nil))
  (do* ((verse (list (caar l)) (append verse (list (car (second l)))))
	(l l (cdr l))
	(sum (cdar l) (incf sum (cdar l))))
       ((or (>= sum n) (= (length l) 1)) (if (= sum n) verse nil))))

(defun find-haiku (l)
  "Scan through a list of conses from make-syllable-sums to see whether there's
a haiku at the beginning of the list.  If not, recursively call ourselves on
the cdr of the list to see if any haikus lurk further along.  Return the
haiku (if found) or nil (if not)."
  (when (equal l nil) (return-from find-haiku nil))
  (let ((line1 (accept-n l 5)))
    (when line1
      (let ((line2 (accept-n (subseq l (length line1)) 7)))
	(when line2
	  (let ((line3 (accept-n (subseq l (+ (length line1) (length line2))) 5)))
	    (when line3
	      (return-from find-haiku (concatenate 'list line1 '("/") line2 '("/") line3))))))))
  (find-haiku (cdr l)))

(defun make-syllable-sums (l)
  "Given a list of words, generate a list of conses of the form (word . syllable-count).
The magic value of 18 is used for any words we didn't find in the CMU dict, because
an 18-syllable word can't be in any part of any haiku."
  (mapcar #'(lambda (w) (cons w (gethash (string-upcase w) *syllable-counts* 18))) l))

(defun make-haiku (context)
  "Generate chains and test them for haikus until you give up.  Returns a list of
strings for the haiku and also a count for the number of attempts made."
  (do* ((n 0 (1+ n))
	(candidate (chain context) (chain context))
	(haiku (find-haiku (make-syllable-sums candidate)) (find-haiku (make-syllable-sums candidate))))
       ((or haiku (> n 20))
	(if haiku
	    (values haiku n) 
	    (values '("With" "apologies" "/"
		      "the" "Muse" "is" "not" "with" "me" "now" "/"
		      "Try" "again" "later.") 20)))))
