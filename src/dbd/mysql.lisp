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
                          :disconnect
                          :ping)
  (:import-from :cl-mysql-system
                :mysql-error
                :connect-to-server
                :mysql-error-message
                :mysql-error-errno
                :release
                :connections
                :in-use
                :return-or-close
                :owner-pool
                :+server-gone-error+))
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
  (call-next-method conn sql :query-class '<dbd-mysql-query>))

(defmethod execute-using-connection ((conn <dbd-mysql-connection>) (query <dbd-mysql-query>) params)
  (let ((result
         (handler-case (query (apply (query-prepared query) params)
                              :database (connection-handle conn)
                              :store nil)
           (mysql-error (e)
             (unwind-protect (error '<dbi-database-error>
                                    :message (mysql-error-message e)
                                    :error-code (mysql-error-errno e))
               ;; KLUDGE: I think this should be done in cl-mysql.
               ;;   cl-mysql doesn't release the connection when a MySQL error has occurred.
               ;;   Though I can't tell which connection is used for the query,
               ;;   I assume the first one is the one.
               (let* ((handle (connection-handle conn))
                      (using-connections (cl-mysql-system:connections handle))
                      (connection (and (> (length using-connections) 0)
                                       (aref using-connections 0))))
                 (when (and connection (in-use connection))
                   (cl-mysql-system:release handle connection))))))))
    (return-or-close (owner-pool result) result)
    (next-result-set result)
    (setf (slot-value query '%result) result)
    query))

(defmethod fetch-using-connection ((conn <dbd-mysql-connection>) query)
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

(defmethod ping ((conn <dbd-mysql-connection>))
  (handler-case (cl-mysql:ping :database (connection-handle conn))
    (mysql-error (e)
      (if (= +server-gone-error+ (mysql-error-errno e))
          nil
          (signal e)))))
