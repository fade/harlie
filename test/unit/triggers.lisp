;;;; test/unit/triggers.lisp
;;;;
;;;; Unit tests for make-name-detector and triggered.

(uiop:define-package #:harlie/test/unit/triggers
  (:use #:cl)
  (:import-from #:harlie
                #:make-name-detector)
  (:local-nicknames (#:tt #:parachute)))

(in-package #:harlie/test/unit/triggers)

(tt:define-test "harlie.unit.triggers")

;;;; ---- make-name-detector -----------------------------------------------

(tt:define-test "make-name-detector"
  :parent "harlie.unit.triggers"

  (let ((detect (make-name-detector "harlie")))

    ;; exact match
    (tt:true (funcall detect "harlie"))

    ;; case-insensitive
    (tt:true (funcall detect "HARLIE"))
    (tt:true (funcall detect "HaRLiE"))

    ;; trailing punctuation allowed
    (tt:true (funcall detect "harlie,"))
    (tt:true (funcall detect "harlie:"))
    (tt:true (funcall detect "harlie!"))

    ;; leading punctuation allowed
    (tt:true (funcall detect ",harlie"))
    (tt:true (funcall detect "@harlie"))

    ;; both sides
    (tt:true (funcall detect ",harlie,"))
    (tt:true (funcall detect "!harlie!"))

    ;; name embedded inside a longer word — must NOT match
    (tt:false (funcall detect "notharlie"))
    (tt:false (funcall detect "harlienot"))
    (tt:false (funcall detect "xharlieyz"))

    ;; completely different word
    (tt:false (funcall detect "bot"))

    ;; empty token
    (tt:false (funcall detect ""))))
