(defpackage #:dbd.mysql
  (:use #:cl
        #:dbi.driver
        #:dbi.logger
        #:dbi.utils
        #:cl-mysql)
  (:shadowing-import-from #:dbi.driver
                          #:disconnect
                          #:ping)
  (:import-from #:dbd.mysql.error
                #:with-error-handler)
  (:import-from #:cl-mysql-system
                #:mysql-error
                #:mysql-error-errno
                #:connect-to-server
                #:return-or-close
                #:owner-pool
                #:+server-gone-error+
                #:+server-lost+)
  (:export #:dbd-mysql
           #:dbd-mysql-connection
           #:dbd-mysql-query
           #:mysql-use-store

           #:<dbd-mysql>
           #:<dbd-mysql-connection>
           #:<dbd-mysql-query>))
(in-package #:dbd.mysql)

(defclass/a dbd-mysql (dbi-driver) ())

(defclass/a dbd-mysql-connection (dbi-connection) ())

(defmethod make-connection ((driver dbd-mysql) &key host database-name username password port socket client-flag)
  (make-instance 'dbd-mysql-connection
     :database-name database-name
     :handle (connect :host host
                      :database database-name
                      :user username
                      :password password
                      :port port
                      :socket socket
                      :client-flag client-flag)))

(defclass dbd-mysql-query (dbi-query)
  ((store :initarg :store :initform t
          :accessor mysql-use-store)))

(defstruct (mysql-result-list (:constructor make-mysql-result-list (&optional result-set row-count)))
  (result-set nil :type list)
  (row-count nil :type integer))

(defmethod prepare ((conn dbd-mysql-connection) (sql string) &key (store t))
  (let ((query (call-next-method conn sql :query-class 'dbd-mysql-query)))
    (setf (mysql-use-store query) store)
    query))

(defun result-set-field-names (handle)
  (mapcar (lambda (field)
            ;; field = (name column-type)
            (intern (first field) :keyword))
          (first (result-set-fields handle))))

(defun fetch-next-row (handle &optional fields)
  (let ((row (next-row handle))
        (fields (or fields
                    (result-set-field-names handle))))
    (when row
      (loop for field in fields
            for value in row
            append (list field value)))))

(defun fetch-all-rows (handle)
  (loop with fields = (result-set-field-names handle)
        for count from 0
        for row = (fetch-next-row handle fields)
        while row
        collect row into rows
        finally (return (values rows
                                (if fields
                                    count
                                    ;; Return the modified count for modification query.
                                    (first (process-result-set handle (make-hash-table))))))))

(defmethod execute-using-connection ((conn dbd-mysql-connection) (query dbd-mysql-query) params)
  (let* (took-usec
         (result
           (with-error-handler conn
             (with-took-usec took-usec
               (query (funcall (query-prepared query) params)
                      :database (connection-handle conn)
                      :store nil)))))
    (return-or-close (owner-pool result) result)
    (next-result-set result)
    (cond
      ((mysql-use-store query)
       (multiple-value-bind (rows count)
           (fetch-all-rows result)
         (sql-log (query-sql query) params count took-usec)
         (setf result (make-mysql-result-list rows count))
         (setf (query-row-count query) count)))
      (t
       (sql-log (query-sql query) params nil took-usec)))
    (setf (query-results query) result)
    query))

(defmethod fetch-using-connection ((conn dbd-mysql-connection) query)
  (let ((result (query-results query)))
    (if (mysql-result-list-p result)
        (pop (slot-value result 'result-set))
        (fetch-next-row result))))

(defmethod escape-sql ((conn dbd-mysql-connection) (sql string))
  (escape-string sql :database (connection-handle conn)))

(defmethod disconnect ((conn dbd-mysql-connection))
  (cl-mysql:disconnect (connection-handle conn)))

(defmethod begin-transaction ((conn dbd-mysql-connection))
  (do-sql conn "START TRANSACTION"))

(defmethod commit ((conn dbd-mysql-connection))
  (do-sql conn "COMMIT"))

(defmethod rollback ((conn dbd-mysql-connection))
  (do-sql conn "ROLLBACK"))

(defmethod ping ((conn dbd-mysql-connection))
  (handler-bind ((mysql-error
                   (lambda (e)
                     (when (or (= (mysql-error-errno e) +server-gone-error+)
                               (= (mysql-error-errno e) +server-lost+))
                       (return-from ping nil)))))
    (cl-mysql:ping :database (connection-handle conn))))

(defmethod row-count ((conn dbd-mysql-connection))
  (second (fetch (execute (prepare conn "SELECT ROW_COUNT()" :store T)))))
