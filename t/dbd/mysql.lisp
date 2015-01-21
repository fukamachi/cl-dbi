(in-package :cl-user)
(defpackage dbd-mysql-test
  (:use :cl
        :cl-test-more
        :dbi))
(in-package :dbd-mysql-test)

(dbi.test:run-driver-tests :mysql :database-name "cl-dbi" :username "nobody" :password "nobody")
