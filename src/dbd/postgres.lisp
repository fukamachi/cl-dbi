#|
  This file is a part of CL-DBI project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage dbd.postgres
  (:use :cl
        :dbi.driver
        :dbi.error
        :cl-postgres)
  (:import-from :cl-postgres-error
                :syntax-error-or-access-violation
                :database-error-message
                :database-error-code))
(in-package :dbd.postgres)

(cl-syntax:use-syntax :annot)

@export
(defclass <dbd-postgres> (<dbi-driver>) ())

@export
(defclass <dbd-postgres-connection> (<dbi-connection>) ())

(defmethod make-connection ((driver <dbd-postgres>) &key database-name username password (host "localhost") (port 5432) (use-ssl :no))
  (make-instance '<dbd-postgres-connection>
     :handle (open-database database-name username password host port use-ssl)))

@export
(defclass <dbd-postgres-query> (<dbi-query>)
     ((name :initarg :name)
      (%result :initform nil)))

@export
(defclass <dbd-postgres-query-result-set> (<dbi-query-result-set>)
  ((field-names :initform '() :accessor result-set-column-names)
   (rows :initform '() :accessor rows)))

(defmethod prepare ((conn <dbd-postgres-connection>) (sql string) &key)
  (let ((name (symbol-name (gensym "PREPARED-STATEMENT"))))
    (setf sql
          (with-output-to-string (s)
            (loop with i = 0
                  with escaped = nil
                  for c across sql
                  if (and (char= c #\\) (not escaped))
                    do (setf escaped t)
                  else do (setf escaped nil)
                  if (and (char= c #\?) (not escaped))
                    do (format s "$~D" (incf i))
                  else do (write-char c s))))
    (handler-case
        (make-instance '<dbd-postgres-query>
           :connection conn
           :name name
           :prepared (prepare-query (connection-handle conn) name sql))
      (syntax-error-or-access-violation (e)
        (error '<dbi-programming-error>
               :message (database-error-message e)
               :error-code (database-error-code e))))))

(defmethod execute ((query <dbd-postgres-query>) params)
  (let ((result (make-instance '<dbd-postgres-query-result-set>
                               :query query)))
    (exec-prepared (connection-handle (query-connection query))
                   (slot-value query 'name)
                   params
                   ;; TODO: lazy fetching
                   (row-reader (fields)
                     (setf (field-names result) (loop
                                                   for field across fields
                                                   collect (field-name field))
                           (rows result) (loop while (next-row)
                                            collect (loop
                                                       for field across fields
                                                       collect (next-field field))))))
    result))

(defmethod fetch-next ((result <dbd-postgres-query-result-set>))
  (pop (rows result)))

(defmethod disconnect ((conn <dbd-postgres-connection>))
  (close-database (connection-handle conn)))

(defmethod begin-transaction ((conn <dbd-postgres-connection>))
  (do-sql conn "BEGIN"))

(defmethod commit ((conn <dbd-postgres-connection>))
  (do-sql conn "COMMIT"))

(defmethod rollback ((conn <dbd-postgres-connection>))
  (do-sql conn "ROLLBACK"))
