(in-package :cl-user)
(defpackage dbd-mysql-test
  (:use :cl
        :cl-test-more
        :dbi))
(in-package :dbd-mysql-test)

(dbi.test:run-driver-tests :mysql
                           :database-name "cl-dbi"
                           :host (or (uiop:getenv "MYSQL_HOST")
                                     "localhost")
                           :port (parse-integer
                                  (or (uiop:getenv "MYSQL_PORT")
                                      "3306"))
                           :username (or (uiop:getenv "MYSQL_USER")
                                         "nobody")
                           :password (or (uiop:getenv "MYSQL_PASS")
                                         "nobody"))
