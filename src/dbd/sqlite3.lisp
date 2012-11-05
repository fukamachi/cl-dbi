#|
  This file is a part of CL-DBI project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)

(defpackage dbd.sqlite3
  (:use :cl
        :dbi.driver
        :dbi.error
        :sqlite)
  (:shadowing-import-from :dbi.driver
                          :disconnect))
(in-package :dbd.sqlite3)

(cl-syntax:use-syntax :annot)

@export
(defclass <dbd-sqlite3> (<dbi-driver>) ())

@export
(defclass <dbd-sqlite3-connection> (<dbi-connection>) ())

@export
(defclass <dbd-sqlite3-query> (<dbi-query>) ())

@export
(defclass <dbd-sqlite3-query-result-set> (<dbi-query-result-set>) ())

(defmethod make-connection ((driver <dbd-sqlite3>) &key database-name)
  (make-instance '<dbd-sqlite3-connection>
     :handle (connect database-name)))

(defmethod prepare ((conn <dbd-sqlite3-connection>) (sql string) &key)
  (handler-case
      (make-instance '<dbd-sqlite3-query>
                     :connection conn
                     :prepared (prepare-statement (connection-handle conn) sql))
    (sqlite-error (e)
      (error '<dbi-database-error>
             :message (sqlite-error-message e)
             :error-code (sqlite-error-code e)))))

(defmethod execute ((query <dbd-sqlite3-query>) params)
  (reset-statement (query-prepared query))
  (clear-statement-bindings (query-prepared query))
  (let ((count 0))
    (dolist (param params)
      (bind-parameter (query-prepared query) (incf count) param)))
  (make-instance '<dbd-sqlite3-query-result-set>
                 :query query))

(defmethod do-sql ((conn <dbd-sqlite3-connection>) (sql string) &rest params)
  (handler-case
      (apply #'execute-non-query (connection-handle conn) sql params)
    (sqlite-error (e)
      (error '<dbi-database-error>
             :message (sqlite-error-message e)
             :error-code (sqlite-error-code e)))))

(defmethod result-set-column-names ((rs <dbd-sqlite3-query-result-set>))
  (loop
     for column in (statement-column-names (query-prepared (result-set-query rs)))
     collect (list column)))

(defmethod fetch-next ((result <dbd-sqlite3-query-result-set>))
  (let ((prepared (query-prepared (result-set-query result))))
    (when (handler-case (step-statement prepared)
            (sqlite-error (e)
              @ignore e
              nil))
      (loop for column in (statement-column-names prepared)
            for i from 0
            collect (statement-column-value prepared i)))))

(defmethod disconnect ((conn <dbd-sqlite3-connection>))
  (sqlite:disconnect (connection-handle conn)))

(defmethod begin-transaction ((conn <dbd-sqlite3-connection>))
  (sqlite:execute-non-query (connection-handle conn) "BEGIN TRANSACTION"))

(defmethod commit ((conn <dbd-sqlite3-connection>))
  (sqlite:execute-non-query (connection-handle conn) "COMMIT TRANSACTION"))

(defmethod rollback ((conn <dbd-sqlite3-connection>))
  (sqlite:execute-non-query (connection-handle conn) "ROLLBACK TRANSACTION"))
