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

(plan 2)

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

(ok (disconnect *db*) "disconnect")

(finalize)
