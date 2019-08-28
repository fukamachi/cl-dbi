(in-package :cl-user)
(defpackage dbd.sqlite3
  (:use :cl
        :dbi.driver
        :dbi.logger
        :dbi.error
        :sqlite
        :annot.class)
  (:shadowing-import-from :dbi.driver
                          :disconnect)
  (:import-from :trivial-garbage
                :finalize)
  (:import-from :uiop/filesystem
                :file-exists-p))
(in-package :dbd.sqlite3)

(cl-syntax:use-syntax :annot)

@export
(defclass <dbd-sqlite3> (<dbi-driver>) ())

@export
(defclass <dbd-sqlite3-connection> (<dbi-connection>) ())

(defmethod make-connection ((driver <dbd-sqlite3>) &key database-name busy-timeout)
  (make-instance '<dbd-sqlite3-connection>
     :database-name database-name
     :handle (connect database-name :busy-timeout busy-timeout)))

@export
@export-accessors
(defclass <dbd-sqlite3-query> (<dbi-query>)
  ((store :initarg :store
          :initform t
          :accessor sqlite3-use-store)))

(defmethod prepare ((conn <dbd-sqlite3-connection>) (sql string) &key (store t))
  (let* ((conn-handle (connection-handle conn))
         (query
           (handler-case
               (make-instance '<dbd-sqlite3-query>
                              :connection conn
                              :prepared (prepare-statement conn-handle sql)
                              :sql sql
                              :store store)
             (sqlite-error (e)
               (if (eq (sqlite-error-code e) :error)
                   (error '<dbi-programming-error>
                          :message (sqlite-error-message e)
                          :error-code (sqlite-error-code e))
                   (error '<dbi-database-error>
                          :message (sqlite-error-message e)
                          :error-code (sqlite-error-code e)))))))
    query))

(defmethod execute-using-connection ((conn <dbd-sqlite3-connection>) (query <dbd-sqlite3-query>) params)
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
                   for row = (fetch-using-connection conn query)
                   while row
                   collect row into rows
                   finally
                   (progn
                     (setf (query-row-count query) count)
                     (sql-log (query-sql query) params count nil)
                     (return rows)))))
      (t (sql-log (query-sql query) params nil nil)))
    query))

(defmethod do-sql ((conn <dbd-sqlite3-connection>) (sql string) &rest params)
  (handler-case
      (apply #'execute-non-query (connection-handle conn) sql params)
    (sqlite-error (e)
      (if (eq (sqlite-error-code e) :error)
          (error '<dbi-programming-error>
                 :message (sqlite-error-message e)
                 :error-code (sqlite-error-code e))
          (error '<dbi-database-error>
                 :message (sqlite-error-message e)
                 :error-code (sqlite-error-code e)))))
  (row-count conn))

(defmethod fetch-using-connection ((conn <dbd-sqlite3-connection>) (query <dbd-sqlite3-query>))
  @ignore conn
  (if (slot-boundp query 'dbi.driver::results)
      (pop (query-results query))
      (let ((prepared (query-prepared query)))
        (when (handler-case (step-statement prepared)
                (sqlite-error (e)
                  @ignore e
                  (finalize-statement prepared)
                  nil))
          (loop for column in (statement-column-names prepared)
                for i from 0
                append (list (intern column :keyword)
                             (statement-column-value prepared i)))))))

(defmethod disconnect ((conn <dbd-sqlite3-connection>))
  (when (slot-boundp (connection-handle conn) 'sqlite::handle)
    (sqlite:disconnect (connection-handle conn))))

(defmethod begin-transaction ((conn <dbd-sqlite3-connection>))
  (sqlite:execute-non-query (connection-handle conn) "BEGIN TRANSACTION")
  (sql-log "BEGIN TRANSACTION" nil nil nil))

(defmethod commit ((conn <dbd-sqlite3-connection>))
  (sqlite:execute-non-query (connection-handle conn) "COMMIT TRANSACTION")
  (sql-log "COMMIT TRANSACTION" nil nil nil))

(defmethod rollback ((conn <dbd-sqlite3-connection>))
  (sqlite:execute-non-query (connection-handle conn) "ROLLBACK TRANSACTION")
  (sql-log "ROLLBACK TRANSACTION" nil nil nil))

(defmethod ping ((conn <dbd-sqlite3-connection>))
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

(defmethod row-count ((conn <dbd-sqlite3-connection>))
  (second (fetch (execute (prepare conn "SELECT changes()")))))

(defmethod free-query-resources ((query <dbd-sqlite3-query>))
  (finalize-statement (query-prepared query)))
