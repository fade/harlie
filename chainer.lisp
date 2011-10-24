;;;; chainer.lisp

(in-package :harlie)

(defun fetch-start (rownum)
  (query (:select 'word3
	  :from 'words
	  :where (:and (:= 'row-num rownum)
		       (:= 'word1 (string #\Newline))
		       (:= 'word2 (string #\Newline)))) :single))

(defun random-start ()
    (let* ((numrows (query (:select (:raw "max(row_num)") :from 'words) :single)))
      (do* ((rownum (random (1+ numrows)) (random (1+ numrows)))
	    (r (fetch-start rownum) (fetch-start rownum))
	    (n 1 (1+ n)))
	   (r (values r n)))))

(defun chain-next (word1 word2)
  (query (:limit
	  (:order-by
	   (:select 'word3
	    :from 'words
	    :where (:and (:= 'word1 word1)
			 (:= 'word2 word2)))
	   (:raw "random()"))
	  1) :single)
  )

(defun chain ()
  (with-connection *chain-db*
    (do* ((word1 (string #\Newline) word2)
	  (word2 (string #\Newline) word3)
	  (word3 (random-start) (chain-next word1 word2))
	  (utterance (list word3)
		     (if (equal word3 (string #\Newline))
			 utterance
			 (append utterance (list word3)))))
	 ((equal word3 (string #\Newline)) utterance)
;      (format t "~A ~A ~A ~A~%" word1 word2 word3 utterance)
      )))

(defun chain-string ()
  (let* ((message (format nil "~A" (chain)))
	 (len (length message)))
    (subseq message 1 (1- len))))
