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
  (make-instance '<dbd-query>
     :connection conn
     :prepared (prepare-statement (connection-handle conn) sql)))

(defmethod execute-using-connection ((conn <dbd-sqlite3-connection>) (query <dbd-query>) params)
  (let ((count 0))
    (dolist (param params)
      (bind-parameter query (incf count) param)))
  query)

(defmethod do-sql ((conn <dbd-sqlite3-connection>) (sql string) &rest params)
  (apply #'execute-non-query (connection-handle conn) sql params))

(defmethod fetch-using-connection ((conn <dbd-sqlite3-connection>) (query <dbd-query>))
  (declare (ignore conn))
  (let ((prepared (query-prepared query)))
    (loop while (step-statement prepared)
          for column in (statement-column-names prepared)
          for i from 0
          append (list (intern column :keyword)
                       (statement-column-value prepared i)))))

(defmethod disconnect ((conn <dbd-sqlite3-connection>))
  (sqlite:disconnect (connection-handle conn)))

(defmethod begin-transaction ((conn <dbd-sqlite3-connection>))
  (sqlite:execute-non-query (connection-handle conn) "BEGIN TRANSACTION"))

(defmethod commit ((conn <dbd-sqlite3-connection>))
  (sqlite:execute-non-query (connection-handle conn) "COMMIT TRANSACTION"))

(defmethod rollback ((conn <dbd-sqlite3-connection>))
  (sqlite:execute-non-query (connection-handle conn) "ROLLBACK TRANSACTION"))
