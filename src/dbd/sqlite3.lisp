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

(defmethod make-connection ((driver <dbd-sqlite3>) &key database-name)
  (make-instance '<dbd-sqlite3-connection>
     :handle (connect database-name)))

(defmethod prepare ((conn <dbd-sqlite3-connection>) (sql string) &key)
  (handler-case
      (make-instance '<dbi-query>
         :connection conn
         :prepared (prepare-statement (connection-handle conn) sql))
    (sqlite-error (e)
      (error '<dbi-database-error>
             :message (sqlite-error-message e)
             :error-code (sqlite-error-code e)))))

(defmethod execute-using-connection ((conn <dbd-sqlite3-connection>) (query <dbi-query>) params)
  (reset-statement (query-prepared query))
  (clear-statement-bindings (query-prepared query))
  (let ((count 0))
    (dolist (param params)
      (bind-parameter (query-prepared query) (incf count) param)))
  query)

(defmethod do-sql ((conn <dbd-sqlite3-connection>) (sql string) &rest params)
  (handler-case
      (apply #'execute-non-query (connection-handle conn) sql params)
    (sqlite-error (e)
      (error '<dbi-database-error>
             :message (sqlite-error-message e)
             :error-code (sqlite-error-code e)))))

(defmethod fetch-using-connection ((conn <dbd-sqlite3-connection>) (query <dbi-query>))
  @ignore conn
  (let ((prepared (query-prepared query)))
    (when (handler-case (step-statement prepared)
            (sqlite-error (e)
              @ignore e
              nil))
      (loop for column in (statement-column-names prepared)
            for i from 0
            append (list (intern column :keyword)
                         (statement-column-value prepared i))))))

(defmethod disconnect ((conn <dbd-sqlite3-connection>))
  (sqlite:disconnect (connection-handle conn)))

(defmethod begin-transaction ((conn <dbd-sqlite3-connection>))
  (sqlite:execute-non-query (connection-handle conn) "BEGIN TRANSACTION"))

(defmethod commit ((conn <dbd-sqlite3-connection>))
  (sqlite:execute-non-query (connection-handle conn) "COMMIT TRANSACTION"))

(defmethod rollback ((conn <dbd-sqlite3-connection>))
  (sqlite:execute-non-query (connection-handle conn) "ROLLBACK TRANSACTION"))
