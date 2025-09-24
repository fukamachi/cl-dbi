(defpackage #:dbd-null-test
  (:use #:cl
        #:dbi.test
        #:rove)
  (:import-from #:dbi
                #:with-connection
                #:do-sql
                #:with-transaction
                #:make-cursor
                #:close-cursor
                #:execute
                #:fetch))
(in-package #:dbd-null-test)

(deftest null-driver-tests
  (apply #'dbi.test:run-driver-tests (list :null)))

(rove:run-test 'null-driver-tests)
