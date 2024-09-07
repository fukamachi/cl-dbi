(defpackage #:dbi-test.dbi
  (:use #:cl
        #:dbi
        #:rove))
(in-package #:dbi-test.dbi)

(deftest disconnect-cached-all
  (ok (= 0
         (hash-table-count (dbi.cache.thread::cache-pool-hash dbi::*threads-connection-pool*))))
  (let ((threads
          (loop repeat 4
                collect
                   (bt2:make-thread
                    (lambda ()
                      (let ((conn
                              (dbi:connect-cached
                               :postgres
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
                                             "nobody"))))
                        (loop
                          (sleep 1))))))))
    (sleep 1)
    (ok (= 4
           (hash-table-count (dbi.cache.thread::cache-pool-hash dbi::*threads-connection-pool*))))
    (dbi:disconnect-cached-all)
    (ok (= 0
           (hash-table-count (dbi.cache.thread::cache-pool-hash dbi::*threads-connection-pool*))))
    (mapc #'bt2:destroy-thread threads)))
