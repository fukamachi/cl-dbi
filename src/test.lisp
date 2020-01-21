(in-package :cl-user)
(defpackage dbi.test
  (:use :cl
        :dbi
        :dbi.driver
        :cl-test-more
        :trivial-types))
(in-package :dbi.test)

(cl-syntax:use-syntax :annot)

(defparameter *db* nil)
(defparameter *driver-name* nil)

@export
(defun run-driver-tests (driver-name &rest params)
  (let ((*package* (find-package :dbi.test))
        (env-var (format nil "SKIP_~A" driver-name))
        (test-name (getf params :test-name)))
    (alexandria:remove-from-plistf params :test-name)
    
    (cond
      ((uiop:getenv env-var)
       (plan 0)
       (finalize))
      (t
       (plan 10)
       (let ((*db* (apply #'connect driver-name params))
             (*driver-name* driver-name))
         (unwind-protect
              (if test-name
                  (run-test test-name)
                  (run-test-package :dbi.test))
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


(deftest |connect|
  (is-type *db* '<dbi-connection>))

(deftest |do-sql|
  (is (do-sql *db* "DROP TABLE IF EXISTS person") 0)
  (is (do-sql *db* "CREATE TABLE person (id INTEGER PRIMARY KEY, name VARCHAR(24) NOT NULL)")
      0)
  (is (do-sql *db* "INSERT INTO person (id, name) VALUES (1, 'fukamachi')")
      1)
  (is (do-sql *db* "INSERT INTO person (id, name) VALUES (2, 'matsuyama')")
      1))

(deftest |prepare, execute & fetch|
  (let (query result)
    (setf query (prepare *db* "SELECT * FROM person"))
    (is-type query '<dbi-query>)
    (setf result (execute query))
    (is (fetch-all result)
        '((:|id| 1 :|name| "fukamachi") (:|id| 2 :|name| "matsuyama")))
    (setf result (execute query))
    (is-type result '<dbi-query>)
    (let ((result (fetch result)))
      (is-type result '(non-nil property-list))
      (is (getf result :|name|) "fukamachi"))
    (let ((result (fetch result)))
      (is-type result '(non-nil property-list))
      (is (getf result :|name|) "matsuyama"))
    (is (fetch result) nil))
  (let* ((query (prepare *db* "SELECT * FROM person WHERE name = ?"))
         (result (execute query nil)))
    (is (fetch result) nil))

  (is-type (execute (prepare *db* "INSERT INTO person (id, name) VALUES (3, 'snmsts')"))
           '<dbi-query>)
  (is (row-count *db*) 1)
  (let* ((query (prepare *db* "SELECT * FROM person WHERE name = ?"))
         (result (execute query "snmsts")))
    (is (getf (fetch result) :|name|) "snmsts")))

(deftest |place holder|
  (let (query result)
    (setf query (prepare *db* "SELECT * FROM person WHERE name = ?"))
    (is-type query '<dbi-query>)
    (setf result (execute query "matsuyama"))
    (is-type result '<dbi-query>)
    (is-type (fetch result) '(non-nil property-list))
    (is (fetch result) nil)))

(deftest |with-transaction|
  (turn-off-autocommit)
  (handler-case
      (progn
        (with-transaction *db*
          (do-sql *db* "INSERT INTO person (id, name) VALUES (4, 'meymao')"))
        (is (fetch (execute (prepare *db* "SELECT * FROM person WHERE name = 'meymao'")))
            '(:|id| 4 :|name| "meymao")))
    (dbi.error:<dbi-notsupported-error> ()
      (skip 1 "Not supported"))))

(deftest |with-nested-transaction|
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

    (dbi.error:<dbi-notsupported-error> ()
      (skip 1 "Not supported"))))

(deftest |duplicate-rollback|
  (turn-off-autocommit)
  (handler-case
      (with-transaction *db*
        (rollback *db*)
        ;; Second rollback should fail
        (prove:is-error (rollback *db*)
                        'dbi.error:<dbi-already-rolled-back-error>
                        "Duplicate rollback should raise an error"))

    (dbi.error:<dbi-notsupported-error> ()
      (skip 1 "Not supported"))))

(deftest |duplicate-commit|
  (turn-off-autocommit)
  (handler-case
      (with-transaction *db*
        (commit *db*)
        ;; Second rollback should fail
        (prove:is-error (commit *db*)
                        'dbi.error:<dbi-already-commited-error>
                        "Duplicate commit should raise an error"))
    
    (dbi.error:<dbi-notsupported-error> ()
      (skip 1 "Not supported"))))

(deftest |code-execution-after-manual-commit|
  (turn-off-autocommit)
  (handler-case
      (let (value)
        (with-transaction *db*
          (commit *db*)
          (setf value :foo))
        (ok (eql value :foo)
            "Value should be set to foo even after the manual commit."))

    (dbi.error:<dbi-notsupported-error> ()
      (skip 1 "Not supported"))))

(deftest |statement error|
  (is-type (handler-case (do-sql *db* "INSERT")
             (error (e) e))
           '<dbi-database-error>)
  (is-type (handler-case (execute (prepare *db* "SELECT SELECT SELECT"))
             (error (e) e))
           '<dbi-database-error>)
  (do-sql *db* "INSERT INTO person (id, name) VALUES (5, 'mizuna')"))
