(defpackage #:dbd.null
  (:use #:cl
        #:dbi.driver
        #:dbi.utils)
  (:export #:dbd-null
           #:dbd-null-connection
           #:dbd-null-query

           #:<dbd-null>
           #:<dbd-null-connection>
           #:<dbd-null-query>))
(in-package #:dbd.null)

(defclass/a dbd-null (cl-dbi:dbi-driver) ())

(defclass/a dbd-null-connection (cl-dbi:dbi-connection) ())

(defclass/a dbd-null-query (cl-dbi:dbi-query) ())

(defmethod cl-dbi:make-connection ((driver dbd-null) &key)
  (make-instance 'dbd-null-connection
                 :handle nil))

(defmethod cl-dbi:prepare ((conn dbd-null-connection) (sql string) &key (store t))
  (make-instance 'dbd-null-query :connection conn :sql sql))

(defmethod cl-dbi:execute-using-connection ((conn dbd-null-connection) (query dbd-null-query) params)
  nil)

(defmethod cl-dbi:do-sql ((conn dbd-null-connection) (sql string) &optional params)
  nil)

(defmethod cl-dbi:fetch-using-connection ((conn dbd-null-connection) (query dbd-null-query))
  nil)

(defmethod cl-dbi:disconnect ((conn dbd-null-connection))
  nil)

(defmethod cl-dbi:begin-transaction ((conn dbd-null-connection))
  nil)

(defmethod cl-dbi:commit ((conn dbd-null-connection))
  nil)

(defmethod cl-dbi:rollback ((conn dbd-null-connection))
  nil)

(defmethod cl-dbi:ping ((conn dbd-null-connection))
  nil)

(defmethod cl-dbi:row-count ((conn dbd-null-connection))
  0)

(defmethod cl-dbi:free-query-resources ((query dbd-null-query))
  nil)
