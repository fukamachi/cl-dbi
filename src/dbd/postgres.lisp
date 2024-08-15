(defpackage #:dbd.postgres
  (:use #:cl
        #:dbi.driver
        #:dbi.logger
        #:dbi.error
        #:dbi.utils
        #:cl-postgres)
  (:import-from #:cl-postgres
                #:connection-socket
                #:send-parse
                #:set-sql-datetime-readers)
  (:import-from #:cl-postgres-error
                #:database-error
                #:syntax-error-or-access-violation
                #:invalid-sql-statement-name

                #:admin-shutdown
                #:crash-shutdown
                #:cannot-connect-now)
  (:shadowing-import-from #:cl-postgres-error
                          #:database-error-message
                          #:database-error-code)
  (:import-from #:trivial-garbage
                #:finalize
                #:cancel-finalization)
  (:export #:dbd-postgres
           #:dbd-postgres-connection
           #:dbd-postgres-query
           #:<dbd-postgres>
           #:<dbd-postgres-connection>
           #:<dbd-postgres-query>))
(in-package #:dbd.postgres)

(defclass/a dbd-postgres (dbi-driver) ())

(defclass/a dbd-postgres-connection (dbi-connection)
  ((%modified-row-count :type (or null fixnum)
                        :initform nil)
   (%deallocation-queue :type list
                        :initform nil)))

(defun get-default-user ()
  (string-right-trim '(#\Newline #\Return)
                     (with-output-to-string (s)
                       (uiop:run-program "psql postgres -c 'select current_user' -t -A"
                                         :output s
                                         :error :interactive))))


(defun usec-convert (usec)
  (+ #.(encode-universal-time 0 0 0 1 1 2000 0)
     (/ usec 1000000.0d0)))

(defmethod make-connection ((driver dbd-postgres) &key database-name username password (host "localhost") (port 5432) (use-ssl :no) (microsecond-precision nil) (service "postgres") (application-name "postmodern-default") use-binary)
  (when microsecond-precision
    (cl-postgres:set-sql-datetime-readers
     :timestamp #'usec-convert
     :timestamp-with-timezone #'usec-convert
     :time #'usec-convert))
  (make-instance 'dbd-postgres-connection
     :database-name database-name
     :handle (open-database database-name
                            (or username (get-default-user))
                            password
                            host
                            port
                            use-ssl
                            service
                            application-name
                            use-binary)))

(defclass/a dbd-postgres-query (dbi-query)
  ((name :initarg :name)
   (freedp :initform nil
           :accessor query-freed-p))
  (:default-initargs
   :fields nil))

(defun set-finalizer (conn query)
  (let ((name (slot-value query 'name)))
    (finalize query
              (lambda ()
                (when (and (database-open-p (connection-handle conn))
                           (not (query-freed-p query)))
                  (push name (slot-value conn '%deallocation-queue)))))))

(defun make-query (conn sql)
  (let* ((conn-handle (connection-handle conn))
         (name (random-string "prepared_stmt"))
         (query
           (make-instance 'dbd-postgres-query
                          :connection conn
                          :name name
                          :sql sql
                          :prepared (prepare-query conn-handle name sql))))
    (set-finalizer conn query)
    query))

(defmacro with-handling-pg-errors (&body body)
  `(handler-case (progn ,@body)
     (syntax-error-or-access-violation (e)
       (error 'dbi-programming-error
              :message (database-error-message e)
              :error-code (database-error-code e)))
     (database-error (e)
       (error 'dbi-database-error
              :message (database-error-message e)
              :error-code (database-error-code e)))))

(defmethod prepare ((conn dbd-postgres-connection) (sql string) &key)
  ;; Deallocate used prepared statements here,
  ;; because GC finalizer may run during processing another query and fail.
  (loop repeat 10 ;; To prevent from freeing many query objects at once
        for prepared = (pop (slot-value conn '%deallocation-queue))
        while prepared
        do (unprepare-query (connection-handle conn) prepared))
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
  (with-handling-pg-errors
    (make-query conn sql)))

(defmethod make-cursor ((conn dbd-postgres-connection) sql &key (cursor-class 'dbi-cursor))
  (make-instance cursor-class
                 :connection conn
                 :sql sql))

(defmethod close-cursor-using-connection ((conn dbd-postgres-connection) (cursor dbi-cursor))
  (when (cursor-declared-p cursor)
    (exec-query (connection-handle conn)
                (format nil "CLOSE ~A" (cursor-name cursor)))
    (setf (cursor-declared-p cursor) nil)
    t))

(defmethod execute-using-connection ((conn dbd-postgres-connection) (cursor dbi-cursor) params)
  (assert (in-transaction conn))
  (with-accessors ((sql query-sql)
                   (name cursor-name)
                   (formatter cursor-formatter))
      cursor
    (exec-query (connection-handle conn)
                (format nil "DECLARE ~A CURSOR FOR ~A"
                        name
                        (funcall formatter params)))
    (setf (query-fields cursor)
          (first
           (exec-query (connection-handle conn)
                       (format nil "FETCH ~A" name)
                       'field-row-reader)))
    (exec-query (connection-handle conn)
                (format nil "FETCH RELATIVE -1 ~A" name)
                'cl-postgres:ignore-row-reader)
    (setf (cursor-declared-p cursor) t)
    cursor))

(defmethod execute-using-connection ((conn dbd-postgres-connection) (query dbd-postgres-query) params)
  (with-handling-pg-errors
    (let (took-usec retried)
      (multiple-value-bind (result count)
          (with-took-usec took-usec
            (block nil
              (tagbody retry
                (handler-case
                    (return (exec-prepared (connection-handle conn)
                                           (slot-value query 'name)
                                           params
                                           (lambda (socket fields)
                                             (let ((result
                                                     (funcall 'list-row-reader socket fields)))
                                               (setf (query-fields query)
                                                     (loop for field across fields
                                                           collect (field-name field)))
                                               (setf (query-results query) result)
                                               query))))
                  (invalid-sql-statement-name (e)
                    ;; Retry if cached prepared statement is not available anymore
                    (when (and (query-cached-p query)
                               (not retried))
                      (assert (eq conn (query-connection query)))
                      (setf (query-prepared query)
                            (prepare-query (connection-handle conn) (random-string "prepared_stmt")
                                           (query-sql query)))
                      (setf retried t)
                      (go retry))
                    (error e))))))
        (sql-log (query-sql query) params count took-usec)
        (or result
            (progn
              (setf (slot-value conn '%modified-row-count) count)
              (make-instance 'dbd-postgres-query
                             :connection conn
                             :sql (query-sql query)
                             :results (list count)
                             :row-count count)))))))

(declaim #+sbcl (sb-ext:muffle-conditions sb-ext:code-deletion-note))
(def-row-reader field-row-reader (fields)
  (loop while (next-row)
        collect (loop for field across fields
                      collect (field-name field)
                      do (next-field field))))
(declaim #+sbcl (sb-ext:unmuffle-conditions sb-ext:code-deletion-note))

(defmethod fetch-using-connection ((conn dbd-postgres-connection) (cursor dbi-cursor))
  (first
   (exec-query (connection-handle conn)
               (format nil "FETCH ~A" (cursor-name cursor))
               'cl-postgres:list-row-reader)))

(defmethod fetch-using-connection ((conn dbd-postgres-connection) (query dbi-query))
  (declare (ignore conn))
  (pop (query-results query)))

(defmethod do-sql ((conn dbd-postgres-connection) sql &optional params)
  (if params
      (progn
        (call-next-method)
        (row-count conn))
      (with-handling-pg-errors
        (let (took-usec)
          (let ((row-count
                  (or (nth-value 1
                                 (with-took-usec took-usec
                                   (exec-query (connection-handle conn) sql)))
                      0)))
            (sql-log sql params row-count took-usec)
            row-count)))))

(defmethod disconnect ((conn dbd-postgres-connection))
  (close-database (connection-handle conn)))

(defmethod begin-transaction ((conn dbd-postgres-connection))
  (do-sql conn "BEGIN"))

(defmethod commit ((conn dbd-postgres-connection))
  (do-sql conn "COMMIT"))

(defmethod rollback ((conn dbd-postgres-connection))
  (do-sql conn "ROLLBACK"))

(defmethod ping ((conn dbd-postgres-connection))
  (let ((handle (connection-handle conn)))
    (handler-case
        (and (database-open-p handle)
             (progn
               (cl-postgres::send-parse (cl-postgres::connection-socket handle)
                                        (random-string "ping")
                                        ""
                                        nil
                                        nil)
               t))
      ((or cl-postgres-error:admin-shutdown
           cl-postgres-error:crash-shutdown
           cl-postgres-error:cannot-connect-now) ()
        nil)
      (error () nil))))

(defmethod free-query-resources ((query dbd-postgres-query))
  (unless (query-freed-p query)
    (unprepare-query (connection-handle (query-connection query)) (slot-value query 'name))
    (setf (query-freed-p query) t)
    (cancel-finalization query)))

(defmethod row-count ((conn dbd-postgres-connection))
  (slot-value conn '%modified-row-count))
