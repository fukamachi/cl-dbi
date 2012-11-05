#|
  This file is a part of CL-DBI project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage dbd.mysql
  (:use :cl
        :dbi.driver
        :dbi.error
        :cl-mysql)
  (:shadowing-import-from :dbi.driver
                          :disconnect)
  (:import-from :cl-mysql-system
                :mysql-error
                :connect-to-server
                :mysql-error-message
                :mysql-error-errno
                :return-or-close
                :owner-pool))
(in-package :dbd.mysql)

(cl-syntax:use-syntax :annot)

@export
(defclass <dbd-mysql> (<dbi-driver>) ())

@export
(defclass <dbd-mysql-connection> (<dbi-connection>) ())

(defmethod make-connection ((driver <dbd-mysql>) &key host database-name username password port socket client-flag)
  (make-instance '<dbd-mysql-connection>
     :handle (connect :host host
                      :database database-name
                      :user username
                      :password password
                      :port port
                      :socket socket
                      :client-flag client-flag)))

@export
(defclass <dbd-mysql-query> (<dbi-query>)
     ((%result :initform nil)))

(defmethod prepare ((conn <dbd-mysql-connection>) (sql string) &key)
  (make-instance '<dbd-mysql-query>
                 :connection conn
                 :prepared (prepare-sql conn sql)))

(defmethod execute ((query <dbd-mysql-query>) params)
  (let ((result
         (handler-case (query (apply (query-prepared query) params)
                              :database (connection-handle (query-connection query))
                              :store nil)
           (mysql-error (e)
             ;; reconnect
             (connect-to-server (slot-value (query-connection query) 'dbi.driver::%handle))
             (error '<dbi-database-error>
                    :message (mysql-error-message e)
                    :error-code (mysql-error-errno e))))))
    (return-or-close (owner-pool result) result)
    (next-result-set result)
    (setf (slot-value query '%result) result)
    query))

(defmethod fetch-next ((query <dbd-mysql-query>))
  (loop with result = (slot-value query '%result)
        for val in (next-row result)
        for (name . type) in (car (result-set-fields result))
        append (list (intern name :keyword) val)))

(defmethod escape-sql ((conn <dbd-mysql-connection>) (sql string))
  (escape-string sql :database (connection-handle conn)))

(defmethod disconnect ((conn <dbd-mysql-connection>))
  (cl-mysql:disconnect (connection-handle conn)))

(defmethod begin-transaction ((conn <dbd-mysql-connection>))
  (do-sql conn "START TRANSACTION"))

(defmethod commit ((conn <dbd-mysql-connection>))
  (do-sql conn "COMMIT"))

(defmethod rollback ((conn <dbd-mysql-connection>))
  (do-sql conn "ROLLBACK"))
