(in-package :cl-user)
(defpackage dbd-postgres-test
  (:use :cl
        :cl-test-more
        :dbi.test))
(in-package :dbd-postgres-test)

(dbi.test:run-driver-tests :postgres
                           :database-name (or (uiop:getenv "POSTGRES_DBNAME")
                                              "cl-dbi")
                           :host (or (uiop:getenv "POSTGRES_HOST")
                                     "localhost")
                           :port (parse-integer
                                  (or (uiop:getenv "POSTGRES_PORT")
                                      "5432"))
                           :username (or (uiop:getenv "POSTGRES_USER")
                                         "nobody")
                           :password (or (uiop:getenv "POSTGRES_PASS")
                                         "nobody"))
