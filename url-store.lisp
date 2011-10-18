;;;; url-store.lisp

(in-package :harlie)

(defclass url-store () ())

(defclass hash-url-store (url-store)
  ((url->short :initform (make-hash-table :test 'equal :synchronized t) :accessor url->short)
   (short->url :initform (make-hash-table :test 'equal :synchronized t) :accessor short->url)
   (url->headline :initform (make-hash-table :test 'equal :synchronized t) :accessor url->headline)))


