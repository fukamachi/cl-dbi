(defpackage #:dbd.sqlite3
  (:use #:cl
        #:dbi.driver
        #:dbi.logger
        #:dbi.error
        #:dbi.utils
        #:sqlite)
  (:shadowing-import-from #:dbi.driver
                          #:disconnect
                          #:with-transaction)
  (:import-from #:trivial-garbage
                #:finalize)
  (:import-from #:uiop/filesystem
                #:file-exists-p)
  (:export #:dbd-sqlite3
           #:dbd-sqlite3-connection
           #:dbd-sqlite3-query
           #:sqlite3-use-store

           #:<dbd-sqlite3>
           #:<dbd-sqlite3-connection>
           #:<dbd-sqlite3-query>))
(in-package #:dbd.sqlite3)

(defclass/a dbd-sqlite3 (dbi-driver) ())

(defclass/a dbd-sqlite3-connection (dbi-connection) ())

(defmethod make-connection ((driver dbd-sqlite3) &key database-name busy-timeout)
  (make-instance 'dbd-sqlite3-connection
     :database-name database-name
     :handle (connect database-name :busy-timeout busy-timeout)))

(defclass/a dbd-sqlite3-query (dbi-query)
  ((store :initarg :store
          :initform t
          :accessor sqlite3-use-store)))

(defmethod prepare ((conn dbd-sqlite3-connection) (sql string) &key (store t))
  (let* ((conn-handle (connection-handle conn))
         (query
           (handler-case
               (make-instance 'dbd-sqlite3-query
                              :connection conn
                              :prepared (prepare-statement conn-handle sql)
                              :sql sql
                              :store store)
             (sqlite-error (e)
               (if (eq (sqlite-error-code e) :error)
                   (error 'dbi-programming-error
                          :message (sqlite-error-message e)
                          :error-code (sqlite-error-code e))
                   (error 'dbi-database-error
                          :message (sqlite-error-message e)
                          :error-code (sqlite-error-code e)))))))
    query))

(defmethod execute-using-connection ((conn dbd-sqlite3-connection) (query dbd-sqlite3-query) params)
  (let ((prepared (query-prepared query)))
    (reset-statement prepared)
    (let ((count 0))
      (dolist (param params)
        (bind-parameter prepared (incf count) param)))
    (slot-makunbound query 'dbi.driver::results)
    (cond
      ((sqlite3-use-store query)
       (setf (query-results query)
             (loop for count from 0
                   for row = (fetch-using-connection conn query :values)
                   while row
                   collect row into rows
                   finally
                   (progn
                     (setf (query-row-count query) count)
                     (sql-log (query-sql query) params count nil)
                     (setf (query-fields query) (statement-column-names prepared))
                     (return rows)))))
      (t (sql-log (query-sql query) params nil nil)))
    query))

(defmethod do-sql ((conn dbd-sqlite3-connection) (sql string) &optional params)
  (let (took-usec)
    (handler-case
        (with-took-usec took-usec
          (apply #'sqlite:execute-non-query (connection-handle conn) sql params))
      (sqlite-error (e)
        (if (eq (sqlite-error-code e) :error)
            (error 'dbi-programming-error
                   :message (sqlite-error-message e)
                   :error-code (sqlite-error-code e))
            (error 'dbi-database-error
                   :message (sqlite-error-message e)
                   :error-code (sqlite-error-code e)))))
    (let ((row-count (row-count conn)))
      (sql-log sql params row-count took-usec)
      (values row-count))))

(defmethod fetch-using-connection ((conn dbd-sqlite3-connection) (query dbd-sqlite3-query) format)
  (declare (ignore conn))
  (if (slot-boundp query 'dbi.driver::results)
      (let ((row (pop (query-results query)))
            (fields (query-fields query)))
        (ecase format
          (:plist
           (loop for field in fields
                 for value in row
                 collect (intern field :keyword)
                 collect value))
          (:alist
           (loop for field in fields
                 for value in row
                 collect (cons field value)))
          (:hash-table
           (let ((hash (make-hash-table :test 'equal)))
             (loop for field in fields
                   for value in row
                   do (setf (gethash field hash) value))
             hash))
          (:values
           row)))
      (let ((prepared (query-prepared query)))
        (when (handler-case (step-statement prepared)
                (sqlite-error (e)
                  (declare (ignore e))
                  (finalize-statement prepared)
                  nil))
          (let ((fields (if (slot-boundp query 'dbi.driver::fields)
                            (query-fields query)
                            (setf (query-fields query)
                                  (statement-column-names prepared)))))
            (ecase format
              (:plist
               (loop for column in fields
                     for i from 0
                     collect (intern column :keyword)
                     collect (statement-column-value prepared i)))
              (:alist
               (loop for column in fields
                     for i from 0
                     collect (cons column (statement-column-value prepared i))))
              (:hash-table
               (let ((hash (make-hash-table :test 'equal)))
                 (loop for column in fields
                       for i from 0
                       do (setf (gethash column hash)
                                (statement-column-value prepared i)))
                 hash))
              (:values
               (loop repeat (length fields)
                     for i from 0
                     collect (statement-column-value prepared i)))))))))

(defmethod disconnect ((conn dbd-sqlite3-connection))
  (when (slot-boundp (connection-handle conn) 'sqlite::handle)
    (sqlite:disconnect (connection-handle conn))))

(defmethod begin-transaction ((conn dbd-sqlite3-connection))
  (sqlite:execute-non-query (connection-handle conn) "BEGIN TRANSACTION")
  (sql-log "BEGIN TRANSACTION" nil nil nil))

(defmethod commit ((conn dbd-sqlite3-connection))
  (sqlite:execute-non-query (connection-handle conn) "COMMIT TRANSACTION")
  (sql-log "COMMIT TRANSACTION" nil nil nil))

(defmethod rollback ((conn dbd-sqlite3-connection))
  (sqlite:execute-non-query (connection-handle conn) "ROLLBACK TRANSACTION")
  (sql-log "ROLLBACK TRANSACTION" nil nil nil))

(defmethod ping ((conn dbd-sqlite3-connection))
  "Return non nil if the database file exists or the database is in-memory.
   The actual  non-nil value  of this  expression is  the path  to the
   database file  in the first  case or  the keyword ':memory'  in the
   second."
  (unless (slot-boundp (connection-handle conn) 'sqlite::handle)
    (return-from ping nil))
  (let* ((handle (connection-handle conn))
         (database-path (sqlite::database-path handle)))
    (cond
      ((string= database-path ":memory:") :memory)
      ((uiop:file-exists-p database-path) database-path)
      (T nil))))

(defmethod row-count ((conn dbd-sqlite3-connection))
  (second (fetch (execute (prepare conn "SELECT changes()")))))

(defmethod free-query-resources ((query dbd-sqlite3-query))
  (finalize-statement (query-prepared query)))
