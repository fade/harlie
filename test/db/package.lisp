;;;; test/db/package.lisp
;;;;
;;;; Shared test-database fixture for harlie DB tests.
;;;; All DB tests connect to "harlie-test" (never "botdb") and clean up
;;;; their own rows so the suite is idempotent and repeatable.

(uiop:define-package #:harlie/test/db/fixture
  (:use #:cl)
  (:import-from #:harlie
                #:make-config
                #:*bot-config*)
  (:import-from #:closer-mop
                #:class-finalized-p
                #:finalize-inheritance)
  (:export #:*test-db*
           #:with-test-db))

(in-package #:harlie/test/db/fixture)

(defparameter *test-db* (list "harlie-test" (uiop:getenv "USER") nil :unix)
  "Postmodern connection spec for the harlie-test database.")

(defun ensure-dao-classes-finalized ()
  "Finalize all harlie DAO classes once at load time so that postmodern
generates insert-dao/update-dao/etc. methods before any test runs them."
  (dolist (class-name '(harlie::bot-channel
                        harlie::harlie-user
                        harlie::channel-user
                        harlie::user-alias))
    (let ((class (find-class class-name nil)))
      (when (and class (not (closer-mop:class-finalized-p class)))
        (closer-mop:finalize-inheritance class)))))

(ensure-dao-classes-finalized)

(defmacro with-test-db (&body body)
  "Bind *BOT-CONFIG* to a minimal test config pointing at harlie-test,
then execute BODY inside a postmodern connection to that database."
  `(let ((*bot-config* (make-config
                        :db-credentials *test-db*
                        :web-server-name "test.localhost"
                        :connections nil)))
     (postmodern:with-connection *test-db*
       ,@body)))
