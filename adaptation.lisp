(in-package #:harlie)
#+sbcl
(defun make-synchronized-hash-table (&key
				       (test 'eql)
				       (size sb-impl::+min-hash-table-size+)
				       (rehash-size 1.5)
				       (rehash-threshold 1))
  (make-hash-table :test test
		   :size size
		   :rehash-size
		   rehash-size
		   :rehash-threshold rehash-threshold
		   :synchronized t))

#+ccl
(defun make-synchronized-hash-table (&key
				       (test 'eql)
				       (size 40)
				       (rehash-size 1.5)
				       (rehash-threshold 1))
  (make-hash-table :test test
		   :size size
		   :rehash-size
		   rehash-size
		   :rehash-threshold rehash-threshold))
