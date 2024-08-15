(defpackage #:dbi.driver
  (:use #:cl
        #:dbi.utils
        #:split-sequence)
  (:import-from #:dbi.error
                #:dbi-error
                #:dbi-unimplemented-error
                #:dbi-notsupported-error)
  (:import-from #:c2mop
                #:class-direct-subclasses)
  (:export #:*row-format*
           #:dbi-driver
           #:dbi-connection
           #:connection-database-name
           #:connection-handle
           #:connection-driver-type
           #:connection-established-at
           #:make-connection
           #:disconnect
           #:find-driver
           #:list-all-drivers
           #:dbi-query
           #:query-connection
           #:query-sql
           #:query-prepared
           #:query-fields
           #:query-results
           #:query-row-count
           #:query-cached-p
           #:dbi-cursor
           #:cursor-name
           #:cursor-formatter
           #:cursor-declared-p
           #:make-cursor
           #:close-cursor
           #:close-cursor-using-connection
           #:prepare
           #:prepare-cached
           #:execute
           #:fetch
           #:fetch-all
           #:fetch-using-connection
           #:do-sql
           #:execute-using-connection
           #:begin-transaction
           #:in-transaction
           #:with-savepoint
           #:with-transaction
           #:commit
           #:rollback
           #:savepoint
           #:rollback-savepoint
           #:release-savepoint
           #:ping
           #:row-count
           #:free-query-resources
           #:escape-sql

           #:<dbi-driver>
           #:<dbi-connection>
           #:<dbi-query>))
(in-package #:dbi.driver)

(defvar *row-format* :plist)

(defclass/a dbi-driver () ()
  (:documentation "Base class for DB driver."))

(defclass/a dbi-connection ()
  ((auto-commit :type boolean
                :initarg :auto-commit
                :initform t)
   (database-name :initarg :database-name
                  :accessor connection-database-name)
   (%handle :initarg :handle
            :accessor connection-handle)
   (query-cache :initform (make-hash-table :test 'equal)
                :accessor connection-query-cache)
   (established-at :initarg :established-at
                   :initform (get-internal-real-time)
                   :accessor connection-established-at))
  (:documentation "Base class for managing DB connection."))

(defgeneric connection-driver-type (conn)
  (:method ((conn dbi-connection))
    (let ((package (package-name (symbol-package (type-of conn)))))
      (cond
        ((string= package #.(string :dbd.mysql))    :mysql)
        ((string= package #.(string :dbd.postgres)) :postgres)
        ((string= package #.(string :dbd.sqlite3))  :sqlite3)))))

(defgeneric make-connection (driver &key)
  (:documentation "Create a instance of `dbi-connection` for the `driver`.
This method must be implemented in each drivers.")
  (:method ((driver dbi-driver) &key)
    (declare (ignore driver))
    (error 'dbi-unimplemented-error
           :method-name 'make-connection)))

(defgeneric disconnect (conn)
  (:method ((conn dbi-connection))
    (declare (ignore conn))
    (error 'dbi-unimplemented-error
           :method-name 'disconnect))
  (:method :after ((conn dbi-connection))
    (setf (connection-query-cache conn) (make-hash-table :test 'equal))))

(defun find-driver (driver-name)
  "Find a driver class named as `driver-name`.
`driver-name` is a string designer.
Driver should be named like 'DBD-SOMETHING' for a database 'something'."
  (or (find (format nil "~A-~A" :dbd driver-name)
            (list-all-drivers)
            :test #'string-equal
            :key #'class-name)
      (find (format nil "<~A-~A>" :dbd driver-name)
            (list-all-drivers)
            :test #'string-equal
            :key #'class-name)))

(defun list-all-drivers ()
  "Return a list of direct subclasses for `dbi-driver`."
  (c2mop:class-direct-subclasses (find-class 'dbi-driver)))

(defclass dbi-query-base ()
  ((connection :type dbi-connection
               :initarg :connection
               :initform nil
               :accessor query-connection)
   (sql :type string
        :initarg :sql
        :accessor query-sql)
   (fields :initarg :fields
           :accessor query-fields)))

(defclass/a dbi-query (dbi-query-base)
  ((prepared :type t
             :initarg :prepared
             :accessor query-prepared)
   (results :initarg :results
            :accessor query-results)
   (row-count :type (or integer null)
              :initarg :row-count
              :initform nil
              :accessor query-row-count)
   (cached :initarg :cached
           :initform nil
           :accessor query-cached-p))
  (:documentation "Class that represents a prepared DB query."))

(defclass dbi-cursor (dbi-query-base)
  ((name :type string
         :initform (random-string "cursor")
         :accessor cursor-name)
   (formatter :type function
              :accessor cursor-formatter)
   (declared :type boolean
             :initform nil
             :accessor cursor-declared-p)))

(defmethod initialize-instance :after ((cursor dbi-cursor) &key sql &allow-other-keys)
  (setf (slot-value cursor 'formatter) (compile-sql sql)))

(defgeneric prepare (conn sql &key))

(defmethod prepare ((conn dbi-connection) (sql string) &key (query-class 'dbi-query))
  "Preparing executing SQL statement and returns a instance of `dbi-query`.
This method may be overrided by subclasses."
  (make-instance query-class
     :connection conn
     :sql sql
     :prepared (prepare-sql conn sql)))

(defun prepare-cached (conn sql &key (query-class 'dbi-query))
  "Same as PREPARE except reusing the created query object."
  (or (gethash sql (connection-query-cache conn))
      (setf (gethash sql (connection-query-cache conn))
            (let ((query (prepare conn sql :query-class query-class)))
              (setf (query-cached-p query) t)
              query))))

(defgeneric make-cursor (conn sql &key cursor-class)
  (:method (conn sql &key cursor-class)
    (declare (ignore conn sql cursor-class))
    (error 'dbi-unimplemented-error
           :method-name 'make-cursor)))

(defgeneric close-cursor (cursor)
  (:method ((cursor dbi-cursor))
    (close-cursor-using-connection (query-connection cursor) cursor)))

(defgeneric execute (query &optional params)
  (:documentation "Execute `query` with `params` and return the results.")
  (:method (object &optional params)
    (execute-using-connection
     (query-connection object)
     object
     params)))

(defgeneric fetch (query &key format)
  (:documentation "Fetch the first row from `query` which is returned by `execute`.")
  (:method (object &key (format *row-format*))
    (let ((values
            (fetch-using-connection (query-connection object) object))
          (fields (query-fields object)))
      (ecase format
        (:plist
         (loop for field in fields
               for value in values
               collect (intern field :keyword)
               collect value))
        (:alist
         (loop for field in fields
               for value in values
               collect (cons field value)))
        (:hash-table
         (loop with hash = (make-hash-table :test 'equal)
               for field in fields
               for value in values
               do (setf (gethash field hash) value)
               finally (return hash)))
        (:values values)))))

(defgeneric fetch-all (query &key format)
  (:documentation "Fetch all rest rows from `query`.")
  (:method ((query dbi-query) &key (format *row-format*))
    (let* (;; Fetch the first row to ensure 'query-fields' is set.
           (first-row (fetch query :format :values))
           (fields (query-fields query))
           (fields (if (eq format :plist)
                       (mapcar (lambda (field) (intern field :keyword)) fields)
                       fields)))
      (loop for row = first-row then (fetch query :format :values)
            while row
            collect
               (ecase format
                 (:plist
                  (loop for key in fields
                        for val in row
                        collect key
                        collect val))
                 (:alist
                  (mapcar #'cons fields row))
                 (:hash-table
                  (let ((hash (make-hash-table :test 'equal)))
                    (loop for key in fields
                          for val in row
                          do (setf (gethash key hash) val))
                    hash))
                 (:values row))))))

(defgeneric fetch-using-connection (conn query)
  (:method ((conn dbi-connection) (query dbi-query))
    (error 'dbi-unimplemented-error
           :method-name 'fetch-using-connection)))

(defgeneric do-sql (conn sql &optional params)
  (:documentation "Do preparation and execution at once.
This method may be overrided by subclasses.")
  (:method ((conn dbi-connection) (sql string) &optional params)
    (let ((query (prepare conn sql)))
      (unwind-protect (progn (execute query params)
                             (query-row-count query))
        (free-query-resources query))))
  (:method :around ((conn dbi-connection) sql &optional params)
    (declare (ignorable sql params))
    (let ((state (get-transaction-state conn)))
      (when state
        (assert-transaction-is-in-progress state))
      (call-next-method))))

(defgeneric execute-using-connection (conn query params)
  (:documentation "Execute `query` in `conn`.
This method must be implemented in each drivers.")
  (:method ((conn dbi-connection) (query dbi-query) params)
    (declare (ignore conn query params))
    (error 'dbi-unimplemented-error
           :method-name 'execute-using-connection)))

(defgeneric close-cursor-using-connection (conn cursor)
  (:method (conn cursor)
    (declare (ignore conn cursor))
    (error 'dbi-unimplemented-error
           :method-name 'close-cursor-using-connection)))

(defgeneric begin-transaction (conn)
  (:documentation "Start a transaction.")
  (:method ((conn dbi-connection))
    (declare (ignore conn))
    (error 'dbi-notsupported-error
           :method-name 'begin-transaction)))

(defmethod begin-transaction :around ((conn dbi-connection))
  "Turn `auto-commit` off automatically before starting a transaction."
  (symbol-macrolet ((auto-commit (slot-value conn 'auto-commit)))
     (let ((saved auto-commit))
       (setf auto-commit nil)
       (unwind-protect (call-next-method)
         (setf auto-commit saved)))))

(defvar *transaction-state* nil
  "A stack of transaction and savepoint states.")

(define-condition transaction-done-condition () ())

(defclass transaction-state ()
  ((conn :type dbi-connection
         :initarg :conn
         :reader get-conn)
   (state :initform :in-progress
          :type (member :in-progress
                        :commited
                        :rolled-back)
          :accessor get-state)))

(defclass savepoint-state (transaction-state)
  ((identifier :type string
               :initform (random-string "savepoint")
               :reader get-identifier)))

(defun get-transaction-state (conn)
  "Returns an object representing transaction's state
   if called inside a transaction block or nil otherwise."
  (find conn *transaction-state*
        :test #'eql
        :key #'get-conn))

(defun in-transaction (conn)
  "Returns True if called inside a transaction block."
  (not (null (get-transaction-state conn))))

(defmacro with-savepoint (conn &body body)
  (let ((ok (gensym "SAVEPOINT-OK"))
        (state-var (gensym "STATE-VAR"))
        (ident-var (gensym "SAVEPOINT-IDENTIFIER-VAR")))
    `(let* ((,state-var (make-instance 'savepoint-state
                                       :conn ,conn))
            (,ident-var (get-identifier ,state-var))
            (*transaction-state*
              (cons ,state-var
                    *transaction-state*))
            ,ok)

       (savepoint ,conn ,ident-var)
       (unwind-protect
            (multiple-value-prog1
                (progn ,@body)
              (setf ,ok t))
         (when (eql (get-state ,state-var)
                    :in-progress)
           (if ,ok
               (release-savepoint ,conn ,ident-var)
               (rollback-savepoint ,conn ,ident-var)))))))

(defmacro %with-transaction (conn &body body)
  (let ((ok (gensym "TRANSACTION-OK"))
        (state-var (gensym "STATE-VAR")))
    `(let* ((state-class (if (in-transaction ,conn)
                             'savepoint-state
                             'transaction-state))
            (,state-var (make-instance state-class
                                       :conn ,conn))
            (*transaction-state*
              (cons ,state-var
                    *transaction-state*))
            ,ok)
       (begin-transaction ,conn)
       (unwind-protect
            (multiple-value-prog1
                (progn ,@body)
              (setf ,ok t))
         (when (eql (get-state ,state-var)
                    :in-progress)
           (if ,ok
               (commit ,conn)
               (rollback ,conn)))))))

(defmacro with-transaction (conn &body body)
  "Start a transaction and commit at the end of this block. If the evaluation `body` is interrupted, the transaction is rolled back automatically."
  (let ((conn-var (gensym "CONN-VAR")))
    `(let ((,conn-var ,conn))
       (if (in-transaction ,conn-var)
           (with-savepoint ,conn-var ,@body)
           (%with-transaction ,conn-var ,@body)))))


(defun assert-transaction-is-in-progress (transaction-state)
  (case (get-state transaction-state)
    (:commited
     (error 'dbi.error:dbi-already-commited-error))
    (:rolled-back
     (error 'dbi.error:dbi-already-rolled-back-error))))

(defgeneric commit (conn)
  (:documentation "Commit changes and end the current transaction.")
  (:method ((conn dbi-connection))
    (declare (ignore conn))
    (error 'dbi-notsupported-error
           :method-name 'commit))
  (:method :around ((conn dbi-connection))
    (let ((state (get-transaction-state conn)))
      (when state
        (assert-transaction-is-in-progress state)

        (multiple-value-prog1
            (etypecase state
              (savepoint-state
               (release-savepoint conn
                                  (get-identifier state)))
              (transaction-state
               (call-next-method)))
          (setf (get-state state)
                :commited))))))

(defgeneric rollback (conn)
  (:documentation "Rollback all changes and end the current transaction.")
  (:method ((conn dbi-connection))
    (declare (ignore conn))
    (error 'dbi-notsupported-error
           :method-name 'rollback))
  (:method :around ((conn dbi-connection))
    (let ((state (get-transaction-state conn)))
      (when state
        (assert-transaction-is-in-progress state)

        (multiple-value-prog1
            (etypecase state
              (savepoint-state
               (rollback-savepoint conn
                                   (get-identifier state)))
              (transaction-state
               (call-next-method)))
          (setf (get-state state)
                :rolled-back))))))

(defgeneric savepoint (conn identifier)
  (:method ((conn dbi-connection) identifier)
    (do-sql conn (format nil "SAVEPOINT ~A" identifier))))

(defmacro finalize-savepoint (new-state &body body)
  `(let ((state (get-transaction-state conn)))
     (unless (typep state 'savepoint-state)
       (error "Please, use release-savepoint inside of with-savepoint block."))
     (unless (equal (get-identifier state)
                    identifier)
       ;; TODO: probably optional `identifier' parameter
       ;;       is excessive and should be removed?
       (error "Identifier mismatch"))

     (multiple-value-prog1
         (progn ,@body)
       (setf (get-state state)
             ,new-state))))

(defgeneric rollback-savepoint (conn &optional identifier)
  (:method ((conn dbi-connection) &optional identifier)
    (do-sql conn (format nil "ROLLBACK TO ~A" identifier)))

  (:method :around ((conn dbi-connection) &optional identifier)
    (finalize-savepoint :rolled-back
      (call-next-method conn identifier))))

(defgeneric release-savepoint (conn &optional identifier)
  (:method ((conn dbi-connection) &optional identifier)
    (do-sql conn (format nil "RELEASE ~A" identifier)))

  (:method :around ((conn dbi-connection) &optional identifier)
    (finalize-savepoint :commited
      (call-next-method conn identifier))))

(defgeneric ping (conn)
  (:documentation
   "Check if the database server is still running and the connection to it is still working.")
  (:method ((conn dbi-connection))
    (declare (ignore conn))
    (error 'dbi-notsupported-error
           :method-name 'ping)))

(defgeneric row-count (conn)
  (:documentation
   "Return the number of counts modified by the last executed INSERT/UPDATE/DELETE query.")
  (:method ((conn dbi-connection))
    (declare (ignore conn))
    (error 'dbi-notsupported-error
           :method-name 'row-count)))

(defgeneric free-query-resources (query)
  (:documentation "Free the resources (e.g. foreign, heap allocated memory)
associated with this query. The default method currently has empty body.")
  (:method (query)) ;; do nothing
  (:method :after (query)
    (when (query-cached-p query)
      (let ((cached-object (gethash (query-sql query)
                                    (connection-query-cache (query-connection query)))))
        (when (eq cached-object query)
          (remhash (query-sql query) (connection-query-cache (query-connection query))))))))

(defgeneric escape-sql (conn sql)
  (:documentation "Return escaped `sql`.
This method may be overrided by subclasses when needed.
For example, in case of MySQL and PostgreSQL, backslashes must be escaped by doubling it.")
  (:method ((conn dbi-connection) (sql string))
    (with-output-to-string (out)
      (loop for c across sql
            if (char= c #\')
              do (write-sequence "''" out)
            else
              do (write-char c out)))))

(defgeneric prepare-sql (conn sql)
  (:documentation
   "Create a function that takes parameters, binds them into a query and returns SQL as a string.")
  (:method ((conn dbi-connection) (sql string))
    (labels ((param-to-sql (param)
               (typecase param
                 (string (concatenate 'string "'" (escape-sql conn param) "'"))
                 (null "NULL")
                 (t (princ-to-string param)))))
      (let ((sql-parts (split-sequence #\? sql)))
        (lambda (params)
          (if params
              (with-output-to-string (out)
                (loop for (part . rest) on sql-parts
                      do (let ((param (pop params)))
                           (write-sequence
                            (if rest
                                (concatenate 'string part (param-to-sql param))
                                part)
                            out))))
              sql))))))
