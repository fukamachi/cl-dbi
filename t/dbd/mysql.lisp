#|
  This file is a part of CL-DBI project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage dbd-mysql-test
  (:use :cl
        :cl-test-more
        :dbi))
(in-package :dbd-mysql-test)

(plan nil)

(defparameter *database-host* "localhost")
(defparameter *database-name* "dbi")
(defparameter *database-user* "root")

(defparameter *db*
              (connect :mysql
                       :host *database-host*
                       :database-name *database-name*
                       :user *database-user*))

(do-sql *db* "DROP TABLE IF EXISTS user")

(do-sql *db* "CREATE TABLE user ( id INTEGER PRIMARY KEY, name VARCHAR(24) NOT NULL )")
(do-sql *db* "INSERT INTO user (id, name) VALUES (1, 'fukamachi')")

(let* ((query (prepare *db* "SELECT name FROM user"))
       (result (execute query)))
  (is (fetch result) '(:|name| "fukamachi") "fetch"))

(do-sql *db* "DROP TABLE user")

(disconnect *db*)

(finalize)
