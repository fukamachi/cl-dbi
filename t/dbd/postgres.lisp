(defpackage #:dbd-postgres-test
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
(in-package #:dbd-postgres-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *connect-args*
    (list :database-name (or (uiop:getenv "POSTGRES_DBNAME")
                             "cl-dbi")
          :host (or (uiop:getenv "POSTGRES_HOST")
                    "localhost")
          :port (parse-integer
                 (or (uiop:getenv "POSTGRES_PORT")
                     "5432"))
          :username (or (uiop:getenv "POSTGRES_USER")
                        "nobody")
          :password (or (uiop:getenv "POSTGRES_PASS")
                        "nobody"))))

(deftest postgres-tests
  (apply #'dbi.test:run-driver-tests :postgres *connect-args*))

(deftest postgres-cursor
  (with-connection #.`(conn :postgres ,@*connect-args*)
    (do-sql conn "DROP TABLE IF EXISTS person")
    (do-sql conn "CREATE TABLE person (id SERIAL PRIMARY KEY, name VARCHAR(24) NOT NULL)")
    (do-sql conn "INSERT INTO person (name) VALUES ('Woody'), ('Buzz'), ('Rex'), ('Hamm'), ('Slinky')")
    (with-transaction conn
      (let ((cursor
              (make-cursor conn "SELECT * FROM person")))
        (execute cursor)
        (ok (equal (getf (fetch cursor) :|name|) "Woody"))
        (ok (equal (getf (fetch cursor) :|name|) "Buzz"))
        (close-cursor cursor)))
    (with-transaction conn
      (let ((cursor (make-cursor conn "SELECT * FROM person WHERE name = 'Trixie'")))
        (execute cursor)
        (ok (null (fetch cursor)))
        (close-cursor cursor)))))
