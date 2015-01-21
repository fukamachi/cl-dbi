(in-package :cl-user)
(defpackage dbd-sqlite3-test
  (:use :cl
        :cl-test-more
        :dbi.test))
(in-package :dbd-sqlite3-test)

(dbi.test:run-driver-tests :sqlite3 :database-name ":memory:")
