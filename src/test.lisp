(defpackage #:dbi.test
  (:use #:cl
        #:dbi
        #:dbi.driver
        #:rove
        #:trivial-types)
  (:import-from #:alexandria
                #:remove-from-plistf)
  (:export #:run-driver-tests))
(in-package #:dbi.test)

(defparameter *db* nil)
(defparameter *driver-name* nil)

(defun run-driver-tests (driver-name &rest params)
  (let ((*package* (find-package '#:dbi.test))
        (env-var (format nil "SKIP_~A" driver-name))
        (test-name (getf params :test-name)))
    (remove-from-plistf params :test-name)

    (cond
      ((uiop:getenv env-var)
       (skip (format nil "Skipped ~A tests" driver-name)))
      (t
       (let ((*db* (apply #'connect driver-name params))
             (*driver-name* driver-name))
         (unwind-protect
              (if test-name
                  (run-test test-name)
                  (run-suite :dbi.test))
           (disconnect *db*)))))))

(defun turn-off-autocommit ()
  (when (eql *driver-name* :mysql)
    (do-sql *db* "SET autocommit=0")))

(defmacro with-collected-queries (&body body)
  "Collects all sql queries produced by the body.
   makes function (get-queries) available in the scope of the block."
  (let ((queries-var (gensym "QUERIES")))
    `(let* (,queries-var
            (dbi:*sql-execution-hooks*
              (cons (lambda (query &rest args)
                      (declare (ignorable args))
                      (format t "QUERY: ~A~%" query)
                      (push query ,queries-var))
                    dbi:*sql-execution-hooks*)))
       (flet ((get-queries ()
                (reverse ,queries-var)))
         ,@body))))

;; Don't know. Why dbi:fetch-all does not work this way?
(defun execute-and-fetch-all (conn sql &rest params)
  (fetch-all
   (apply #'execute
          (prepare conn sql)
          params)))

(deftest connect
  (ok (typep *db* 'dbi-connection)))

(deftest do-sql
  (ok (eql (do-sql *db* "DROP TABLE IF EXISTS person") 0))
  (ok (eql (do-sql *db* "CREATE TABLE person (id INTEGER PRIMARY KEY, name VARCHAR(24) NOT NULL)")
           0))
  (ok (eql (do-sql *db* "INSERT INTO person (id, name) VALUES (1, 'fukamachi')")
           1))
  (ok (eql (do-sql *db* "INSERT INTO person (id, name) VALUES (2, 'matsuyama')")
           1)))

(deftest prepare-execute-fetch
  (let (query result)
    (setf query (prepare *db* "SELECT * FROM person"))
    (ok (typep query 'dbi-query))
    (setf result (execute query))
    (ok (equal (fetch-all result)
               '((:|id| 1 :|name| "fukamachi") (:|id| 2 :|name| "matsuyama"))))
    (setf result (execute query))
    (ok (typep result 'dbi-query))
    (let ((result (fetch result)))
      (ok (typep result '(non-nil property-list)))
      (ok (equal (getf result :|name|) "fukamachi")))
    (let ((result (fetch result)))
      (ok (typep result '(non-nil property-list)))
      (ok (equal (getf result :|name|) "matsuyama")))
    (ok (null (fetch result))))
  (let* ((query (prepare *db* "SELECT * FROM person WHERE name = ?"))
         (result (execute query nil)))
    (ok (null (fetch result))))

  (ok (typep (execute (prepare *db* "INSERT INTO person (id, name) VALUES (3, 'snmsts')"))
             'dbi-query))
  (ok (eql (row-count *db*) 1))
  (let* ((query (prepare *db* "SELECT * FROM person WHERE name = ?"))
         (result (execute query "snmsts")))
    (ok (equal (getf (fetch result) :|name|) "snmsts"))))

(deftest place-holder
  (let (query result)
    (setf query (prepare *db* "SELECT * FROM person WHERE name = ?"))
    (ok (typep query 'dbi-query))
    (setf result (execute query "matsuyama"))
    (ok (typep result 'dbi-query))
    (ok (typep (fetch result) '(non-nil property-list)))
    (ok (null (fetch result)))))

(deftest with-transaction
  (turn-off-autocommit)
  (handler-case
      (progn
        (with-transaction *db*
          (do-sql *db* "INSERT INTO person (id, name) VALUES (4, 'meymao')"))
        (ok (equal (fetch (execute (prepare *db* "SELECT * FROM person WHERE name = 'meymao'")))
                   '(:|id| 4 :|name| "meymao"))))
    (dbi.error:dbi-notsupported-error ()
      (skip "Not supported"))))

(deftest with-nested-transaction
  "Nested transactions should be transformed into savepoints"
  (turn-off-autocommit)

  (handler-case
      (flet ((get-row-number ()
               (getf (first
                      (execute-and-fetch-all *db* "SELECT COUNT(*) as cnt FROM person"))
                     :|cnt|)))
        (with-transaction *db*
          (do-sql *db* "DROP TABLE IF EXISTS person")
          (do-sql *db* "CREATE TABLE person (id INTEGER PRIMARY KEY, name VARCHAR(24) NOT NULL)")

          (do-sql *db* "INSERT INTO person (id, name) VALUES (1, 'foo')")
          (with-transaction *db*
            (do-sql *db* "INSERT INTO person (id, name) VALUES (2, 'bar')")
            ;;
            (ok (= (get-row-number) 2)
                "At this stage there should be two items in the database")

            ;; This should rollback the second INSERT
            (rollback *db*)

            (ok (= (get-row-number) 1)
                "And now we have only one row"))))

    (dbi.error:dbi-notsupported-error ()
      (skip "Not supported"))))

(deftest duplicate-rollback
  (turn-off-autocommit)
  (handler-case
      (with-transaction *db*
        (rollback *db*)
        ;; Second rollback should fail
        (ok (signals (rollback *db*)
                     'dbi.error:dbi-already-rolled-back-error)
            "Duplicate rollback should raise an error"))

    (dbi.error:dbi-notsupported-error ()
      (skip "Not supported"))))

(deftest select-after-rollback
    (turn-off-autocommit)
  (handler-case
      (with-transaction *db*
        (rollback *db*)
        ;; Attempt to execute a SELECT after the manual rollback should fail
        (ok (signals (do-sql *db* "SELECT 1")
                     'dbi.error:dbi-already-rolled-back-error)
            "SQL statements should fail after the manual rollback"))

    (dbi.error:dbi-notsupported-error ()
      (skip "Not supported"))))

(deftest select-after-commit
  (turn-off-autocommit)
  (handler-case
      (with-transaction *db*
        (commit *db*)
        ;; Attempt to execute a SELECT after the manual commit should fail
        (ok (signals (do-sql *db* "SELECT 1")
                     'dbi.error:dbi-already-commited-error)
            "SQL statements should fail after the manual commit"))

    (dbi.error:dbi-notsupported-error ()
      (skip "Not supported"))))

(deftest duplicate-commit
  (turn-off-autocommit)
  (handler-case
      (with-transaction *db*
        (commit *db*)
        ;; Second rollback should fail
        (ok (signals (commit *db*)
                     'dbi.error:dbi-already-commited-error)
            "Duplicate commit should raise an error"))

    (dbi.error:dbi-notsupported-error ()
      (skip "Not supported"))))

(deftest code-execution-after-manual-commit
  (turn-off-autocommit)
  (handler-case
      (let (value)
        (with-transaction *db*
          (commit *db*)
          (setf value :foo))
        (ok (eql value :foo)
            "Value should be set to foo even after the manual commit."))

    (dbi.error:dbi-notsupported-error ()
      (skip "Not supported"))))

(deftest statement-error
  (ok (typep (handler-case (do-sql *db* "INSERT")
               (error (e) e))
             'dbi-database-error))
  (ok (typep (handler-case (execute (prepare *db* "SELECT SELECT SELECT"))
               (error (e) e))
             'dbi-database-error))
  (do-sql *db* "INSERT INTO person (id, name) VALUES (5, 'mizuna')"))
