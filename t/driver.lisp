(defpackage #:dbi-test.driver
  (:use #:cl
        #:rove
        #:dbi
        #:dbi.driver))
(in-package #:dbi-test.driver)

(setup
  (when (find-class 'dbd-imaginedb nil)
    (setf (find-class 'dbd-imaginedb) nil))

  (when (find-class '<dbd-imaginedb> nil)
    (setf (find-class '<dbd-imaginedb>) nil)))

(deftest unknown-driver
  (ng (find-driver :imaginedb)
      "find-driver: which doesn't exist"))

(deftest custom-driver
  (locally
    #+sbcl (declare (sb-ext:muffle-conditions style-warning))
    (handler-bind ((style-warning #'muffle-warning))
      (defclass dbd-imaginedb (dbi-driver) ())
      (defmethod make-connection ((class dbd-imaginedb) &rest params)
        (declare (ignore params))
        (make-instance 'dbi-connection))

      (defclass <dbd-imaginedb2> (<dbi-driver>) () )
      (defmethod make-connection ((class <dbd-imaginedb2>) &rest params)
        (declare (ignore params))
        (make-instance '<dbi-connection>))))

  (ok (c2mop:subclassp (find-driver :imaginedb) 'dbi-driver)
      "find-driver: which exists")
  (ok (c2mop:subclassp (find-driver :imaginedb2) 'dbi-driver)
      "find-driver: which exists")

  (ok (find (find-class 'dbd-imaginedb) (list-all-drivers))
      "list-all-drivers")
  (ok (find (find-class '<dbd-imaginedb2>) (list-all-drivers))
      "list-all-drivers")

  (let ((conn (connect :imaginedb))
        (conn2 (connect :imaginedb2)))

    (ok (typep conn 'dbi-connection)
        "connect")
    (ok (typep conn2 '<dbi-connection>)
        "connect")

    (let ((query (prepare conn "SELECT * FROM kyoto WHERE type = ?"))
          (query2 (prepare conn2 "SELECT * FROM kyoto WHERE type = ?")))

      (ok (typep query 'dbi-query)
          "prepare")
      (ok (typep query2 '<dbi-query>)
          "prepare")

      (ok (equal (funcall (query-prepared query) "cafe")
                 "SELECT * FROM kyoto WHERE type = 'cafe'")
          "prepare-sql")
      (ok (equal (funcall (query-prepared query2) "cafe")
                 "SELECT * FROM kyoto WHERE type = 'cafe'")
          "prepare-sql"))))
