(in-package #:harlie)

(declaim (optimize (debug 3) (safety 3) (speed 0)))

(defparameter *crons* nil)

(defun happy-new-year ()
  (say (format nil "Happy New Years ~D, everyone!"
               (timestamp-year (now)))  :public t))

(defun add-canonical-crons ()
  (push (make-cron-job #'happy-new-year :month 1 :day-of-month 1 :hour 0 :minute 0) *crons*))
