(in-package #:harlie)

;; This mechanism written by Nikodemus Siivola <nikodemus@random-state.net>

(defclass task ()
  ((thread :initform nil :reader task-thread)
   (lock :initform (bordeaux-threads:make-lock "Task Lock") :reader task-lock)
   (control-lock :initform (bordeaux-threads:make-lock "Task Control Lock") :accessor task-control-lock)
   (name :initarg :name :reader task-name)
   (function :initarg :function :reader task-function)
   (interval :initarg :interval :accessor task-interval)))

(defun stop-task (task)
  (bordeaux-threads:with-recursive-lock-held ((task-control-lock task))
    (bordeaux-threads:with-recursive-lock-held ((task-lock task))
      (setf (slot-value task 'interval) nil))
    (let ((thread (task-thread task)))
      (when thread
        (bordeaux-threads:join-thread thread)))
    (setf (slot-value task 'thread) nil)))

(defun start-task (task interval &optional (function (task-function task)))
  (tagbody
     (format t "Entering start-task~%")
     (bordeaux-threads:with-recursive-lock-held ((task-control-lock task))
       (format t "Inside recursive lock in start-task~%")
       (when (task-interval task)
         (let ((thread (task-thread task)))
           (when (and thread (bordeaux-threads:thread-alive-p thread))
             (go :oops))))
       (setf (slot-value task 'interval) interval
             (slot-value task 'function) function
             (slot-value task 'thread)
             (bordeaux-threads:make-thread
              (lambda ()
		(format t "Entering outer thunk.~%")
                (loop for s = (bordeaux-threads:with-recursive-lock-held ((task-lock task))
                                (task-interval task))
                      while s
                      do (sleep s)
                         (funcall (task-function task)))
		(format t "Outer thunk exiting.~%"))
              :name (task-name task))))
     (return-from start-task task)
   :oops
     (error "Task already running.")))

(defmethod initialize-instance :after ((task task) &key)
  (let ((interval (task-interval task)))
    (when interval
      (start-task task interval))))

(defun make-task (function interval name)
  (make-instance 'task :function function :interval interval :name name))
