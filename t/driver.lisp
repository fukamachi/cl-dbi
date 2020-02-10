(defpackage #:dbi-test.driver
  (:use #:cl
        #:rove
        #:dbi
        #:dbi.driver))
(in-package #:dbi-test.driver)

(ng (find-driver :imaginedb)
    "find-driver: which doesn't exist")

(defclass <dbd-imaginedb> (<dbi-driver>) () )
(defmethod make-connection ((class <dbd-imaginedb>) &rest params)
  (declare (ignore params))
  (make-instance '<dbi-connection>))

(ok (c2mop:subclassp (find-driver :imaginedb) '<dbi-driver>)
    "find-driver: which exists")

(ok (find (find-class '<dbd-imaginedb>) (list-all-drivers))
    "list-all-drivers")

(defparameter *connection* (connect :imaginedb))

(ok (typep *connection* '<dbi-connection>)
    "connect")

(defparameter *query*
              (prepare *connection* "SELECT * FROM kyoto WHERE type = ?"))

(ok (typep *query* '<dbi-query>)
    "prepare")

(ok (equal (funcall (slot-value *query* 'dbi.driver::prepared) "cafe")
           "SELECT * FROM kyoto WHERE type = 'cafe'")
    "prepare-sql")

(c2mop:remove-direct-subclass (find-class '<dbi-driver>) (find-class '<dbd-imaginedb>))
