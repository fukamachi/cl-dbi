(in-package :cl-user)
(defpackage dbi.driver
  (:use :cl
        :annot.class
        :split-sequence)
  (:import-from :c2mop
                :class-direct-subclasses)
  (:import-from :dbi.error
                :<dbi-unimplemented-error>
                :<dbi-notsupported-error>))
(in-package :dbi.driver)

(cl-syntax:use-syntax :annot)

(declaim (optimize (speed 3)))

@export
(defclass <dbi-driver> () ()
  (:documentation "Base class for DB driver."))

@export
@export-accessors
(defclass <dbi-connection> ()
     ((auto-commit :type boolean
                   :initarg :auto-commit
                   :initform t)
      (database-name :initarg :database-name
                     :accessor connection-database-name)
      (%handle :initarg :handle
               :accessor connection-handle))
  (:documentation "Base class for managing DB connection."))

@export
(defgeneric connection-driver-type (conn)
  (:method ((conn <dbi-connection>))
    (let ((package (package-name (symbol-package (type-of conn)))))
      (cond
        ((string= package #.(string :dbd.mysql))    :mysql)
        ((string= package #.(string :dbd.postgres)) :postgres)
        ((string= package #.(string :dbd.sqlite3))  :sqlite3)))))

@export
(defgeneric make-connection (driver &key)
  (:documentation "Create a instance of `<dbi-connection>` for the `driver`.
This method must be implemented in each drivers.")
  (:method ((driver <dbi-driver>) &key)
    @ignore driver
    (error '<dbi-unimplemented-error>
           :method-name 'make-connection)))

@export
(defgeneric disconnect (conn)
  (:method ((conn <dbi-connection>))
    @ignore conn
    (error '<dbi-unimplemented-error>
           :method-name 'disconnect)))

@export
(defun find-driver (driver-name)
  "Find a driver class named as `driver-name`.
`driver-name` is a string designer.
Driver should be named like '<DBD-SOMETHING>' for a database 'something'."
  (find (format nil "<DBD-~:@(~A~)>" driver-name)
        (list-all-drivers)
        :test #'string=
        :key #'class-name))

@export
(defun list-all-drivers ()
  "Return a list of direct subclasses for `<dbi-driver>`."
  (c2mop:class-direct-subclasses (find-class '<dbi-driver>)))

@export
@export-accessors
(defclass <dbi-query> ()
  ((connection :type <dbi-connection>
               :initarg :connection
               :initform nil
               :accessor query-connection)
   (sql :type string
        :initarg :sql
        :accessor query-sql)
   (prepared :type t
             :initarg :prepared
             :accessor query-prepared)
   (results :initarg :results
            :accessor query-results)
   (row-count :type (or integer null)
              :initarg :row-count
              :initform nil
              :accessor query-row-count))
  (:documentation "Class that represents a prepared DB query."))

@export
(defgeneric prepare (conn sql &key))

@export
(defmethod prepare ((conn <dbi-connection>) (sql string) &key (query-class '<dbi-query>))
  "Preparing executing SQL statement and returns a instance of `<dbi-query>`.
This method may be overrided by subclasses."
  (make-instance query-class
     :connection conn
     :sql sql
     :prepared (prepare-sql conn sql)))

@export
(defgeneric execute (query &rest params)
  (:documentation "Execute `query` with `params` and return the results.")
  (:method ((query <dbi-query>) &rest params)
    (execute-using-connection
     (query-connection query)
     query
     params)))

@export
(defgeneric fetch (query)
  (:documentation "Fetch the first row from `query` which is returned by `execute`.")
  (:method ((query <dbi-query>))
    (fetch-using-connection (query-connection query) query)))

@export
(defgeneric fetch-all (query)
  (:documentation "Fetch all rest rows from `query`.")
  (:method ((query <dbi-query>))
    (loop for result = (fetch query)
          while result
          collect result)))

@export
(defgeneric fetch-using-connection (conn query)
  (:method ((conn <dbi-connection>) (query <dbi-query>))
    (error '<dbi-unimplemented-error>
           :method-name 'fetch-using-connection)))

@export
(defgeneric do-sql (conn sql &rest params)
  (:documentation "Do preparation and execution at once.
This method may be overrided by subclasses.")
  (:method ((conn <dbi-connection>) (sql string) &rest params)
    (let ((query (prepare conn sql)))
      (apply #'execute query params)
      (query-row-count query))))

@export
(defgeneric execute-using-connection (conn query params)
  (:documentation "Execute `query` in `conn`.
This method must be implemented in each drivers.")
  (:method ((conn <dbi-connection>) (query <dbi-query>) params)
    @ignore (conn query params)
    (error '<dbi-unimplemented-error>
           :method-name 'execute-using-connection)))

(defvar *in-transaction* nil)

@export
(defgeneric begin-transaction (conn)
  (:documentation "Start a transaction.")
  (:method ((conn <dbi-connection>))
    @ignore conn
    (error '<dbi-notsupported-error>
           :method-name 'begin-transaction)))

@export
(defmethod begin-transaction :around ((conn <dbi-connection>))
  "Turn `auto-commit` off automatically before starting a transaction."
  (symbol-macrolet ((auto-commit (slot-value conn 'auto-commit)))
     (let ((saved auto-commit))
       (setf auto-commit nil)
       (unwind-protect (call-next-method)
         (setf auto-commit saved)))))

@export
(defvar *in-transaction* nil)

@export
(defvar *current-savepoint* nil)

(define-condition transaction-done-condition () ())

@export
(defgeneric commit (conn)
  (:documentation "Commit changes and end the current transaction.")
  (:method ((conn <dbi-connection>))
    @ignore conn
    (error '<dbi-notsupported-error>
           :method-name 'commit))
  (:method :around ((conn <dbi-connection>))
    (multiple-value-prog1
        (if *current-savepoint*
            (release-savepoint conn *current-savepoint*)
            (call-next-method))
      (when *in-transaction*
        (error 'transaction-done-condition)))))

@export
(defgeneric rollback (conn)
  (:documentation "Rollback all changes and end the current transaction.")
  (:method ((conn <dbi-connection>))
    @ignore conn
    (error '<dbi-notsupported-error>
           :method-name 'rollback))
  (:method :around ((conn <dbi-connection>))
    (multiple-value-prog1
        (if *current-savepoint*
            (rollback-savepoint conn *current-savepoint*)
            (call-next-method))
      (when *in-transaction*
        (error 'transaction-done-condition)))))

@export
(defgeneric savepoint (conn identifier)
  (:method ((conn <dbi-connection>) identifier)
    (do-sql conn (format nil "SAVEPOINT ~A" identifier))))

@export
(defgeneric rollback-savepoint (conn &optional identifier)
  (:method ((conn <dbi-connection>) &optional (identifier *current-savepoint*))
    (do-sql conn (format nil "ROLLBACK TO ~A" identifier)))
  (:method :after ((conn <dbi-connection>) &optional identifier)
    (declare (ignore identifier))
    (error 'transaction-done-condition)))

@export
(defgeneric release-savepoint (conn &optional identifier)
  (:method ((conn <dbi-connection>) &optional (identifier *current-savepoint*))
    (do-sql conn (format nil "RELEASE ~A" identifier)))
  (:method :after ((conn <dbi-connection>) &optional identifier)
    (declare (ignore identifier))
    (error 'transaction-done-condition)))

@export
(defgeneric ping (conn)
  (:documentation
   "Check if the database server is still running and the connection to it is still working.")
  (:method ((conn <dbi-connection>))
    @ignore conn
    (error '<dbi-notsupported-error>
           :method-name 'ping)))

@export
(defgeneric row-count (conn)
  (:documentation
   "Return the number of counts modified by the last executed INSERT/UPDATE/DELETE query.")
  (:method ((conn <dbi-connection>))
    @ignore conn
    (error '<dbi-notsupported-error>
           :method-name 'row-count)))

@export
(defgeneric free-query-resources (query)
  (:documentation "Free  the resources (e.g.  foreign,  heap allocated
  memory) associated with  this query. This method  is specialized and
  effective (and should matches each 'prepare' function call) only for
  <dbd-sqlite3-query>. The default method currently has empty body.")
  (:method (query))) ;; does nothing

@export
(defgeneric escape-sql (conn sql)
  (:documentation "Return escaped `sql`.
This method may be overrided by subclasses when needed.
For example, in case of MySQL and PostgreSQL, backslashes must be escaped by doubling it.")
  (:method ((conn <dbi-connection>) (sql string))
    (with-output-to-string (out)
      (loop for c across sql
            if (char= c #\')
              do (write-sequence "''" out)
            else
              do (write-char c out)))))

(defgeneric prepare-sql (conn sql)
  (:documentation
   "Create a function that takes parameters, binds them into a query and returns SQL as a string.")
  (:method ((conn <dbi-connection>) (sql string))
    (labels ((param-to-sql (param)
               (typecase param
                 (string (concatenate 'string "'" (escape-sql conn param) "'"))
                 (null "NULL")
                 (t (princ-to-string param)))))
      (let ((sql-parts (split-sequence #\? sql)))
        (lambda (&rest params)
          (if params
              (with-output-to-string (out)
                (loop for (part . rest) on sql-parts
                      do
                         (let ((param (pop params)))
                           (write-sequence
                            (if rest
                                (concatenate 'string part (param-to-sql param))
                                part)
                            out))))
              sql))))))
