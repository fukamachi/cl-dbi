#|
  This file is a part of CL-DBI project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage dbd-sqlite3-test
  (:use :cl
        :cl-test-more
        :dbi))
(in-package :dbd-sqlite3-test)

(plan 5)

(defvar *db-path*
    (asdf:system-relative-pathname
     (asdf:find-system :dbd-sqlite3)
     #p"t/dbd/tmp.db"))

(ignore-errors (delete-file *db-path*))

(defparameter *db* (connect :sqlite3 :database-name *db-path*))

(do-sql *db* "CREATE TABLE user ( id INTEGER PRIMARY KEY, name VARCHAR(24) NOT NULL )")
(do-sql *db* "INSERT INTO user (id, name) VALUES (1, 'fukamachi')")

(let* ((query (prepare *db* "SELECT name FROM user"))
       (result (execute query)))
  (is (fetch result) '(:|name| "fukamachi") "fetch"))

(begin-transaction *db*)
(do-sql *db* "INSERT INTO user (id, name) VALUES (2, 'matsuyama')")
(commit *db*)

(let* ((query (prepare *db* "SELECT name FROM user WHERE name = 'matsuyama'"))
       (result (execute query)))
  (is (fetch result)
      '(:|name| "matsuyama")
      "begin-transaction & commit"))

(with-transaction *db*
  (do-sql *db* "INSERT INTO user (id, name) VALUES (3, 'meymao')"))

(let* ((query (prepare *db* "SELECT name FROM user WHERE name = 'meymao'"))
       (result (execute query)))
  (is (fetch result)
      '(:|name| "meymao")
      "with-transaction"))

;; raise a simple-error to test `rollback'.
(ignore-errors
  (with-transaction *db*
    (do-sql *db* "INSERT INTO user (id, name) VALUES (4, 'foobar')")
    (error 'simple-error)))

(let* ((query (prepare *db* "SELECT name FROM user WHERE name = 'foobar'"))
       (result (execute query)))
  (is (fetch result)
      nil
      "with-transaction & rollback"))

(ok (disconnect *db*) "disconnect")

(finalize)
