(defpackage #:dbd-sqlite3-test
  (:use #:cl
        #:dbi.test
        #:rove))
(in-package #:dbd-sqlite3-test)

(deftest sqlite3-tests
  (dbi.test:run-driver-tests :sqlite3 :database-name ":memory:"))
