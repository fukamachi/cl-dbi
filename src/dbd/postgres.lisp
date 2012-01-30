#|
  This file is a part of CL-DBI project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage dbd.postgres
  (:use :cl
        :dbi.driver
        :dbi.error
        :cl-postgres))
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
(defclass <dbd-postgres-query> (<dbd-query>)
     ((name :initarg :name)
      (%result :initform nil)))

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
    (make-instance '<dbd-postgres-query>
       :connection conn
       :name name
       :prepared (prepare-query (connection-handle conn) name sql))))

(defmethod execute-using-connection ((conn <dbd-postgres-connection>) (query <dbd-postgres-query>) params)
  (exec-prepared (connection-handle conn)
                 (slot-value query 'name)
                 params
                 ;; TODO: lazy fetching
                 (row-reader (fields)
                   (let ((result
                          (loop while (next-row)
                                collect (loop for field across fields
                                              collect (intern (field-name field) :keyword)
                                              collect (next-field field)))))
                     (setf (slot-value query '%result)
                           result)
                     query))))

(defmethod fetch ((query <dbd-postgres-query>))
  (pop (slot-value query '%result)))

(defmethod begin-transaction ((conn <dbd-postgres-connection>))
  (do-sql conn "BEGIN"))

(defmethod commit ((conn <dbd-postgres-connection>))
  (do-sql conn "COMMIT"))

(defmethod rollback ((conn <dbd-postgres-connection>))
  (do-sql conn "ROLLBACK"))
