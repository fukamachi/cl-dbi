#|
  This file is a part of CL-DBI project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage dbi.driver
  (:use :cl
        :split-sequence
        :alexandria)
  (:import-from :c2mop
                :class-direct-subclasses)
  (:import-from :dbi.error
                :<dbi-unimplemented-error>
                :<dbi-notsupported-error>)
  (:export :connection-handle
           :query-connection
           :query-prepared
           :result-set-query
           :result-set-column-names))
(in-package :dbi.driver)

(cl-syntax:use-syntax :annot)

(declaim (optimize (speed 3)))

@export
(defclass <dbi-driver> () ()
  (:documentation "Base class for DB driver."))

@export
(defclass <dbi-connection> ()
     ((auto-commit :type boolean
                   :initarg :auto-commit
                   :initform t)
      (%handle :initarg :handle
               :accessor connection-handle))
  (:documentation "Base class for managing DB connection."))

(defmacro define-dbi-interface (name (class &rest args) docstring)
  "Define NAME as a method which a DBD driver must implement.

Creates a generic function, with :documentation DOCSTRING, and a
default method with simply signals a <dbi-unimplemented-error>
condition."
  (multiple-value-bind (required optional rest keys)
      (parse-ordinary-lambda-list args)
    `(progn
       (defgeneric ,name (,class ,@args)
         (:documentation ,docstring)
         (:method ((object ,class) ,@args)
           (declare (ignore ,@required
                            ,@(mapcar #'first optional)
                            ,@(if rest (list rest) '())
                            ,@(mapcar (compose #'second #'first) keys)))
           (error '<dbi-unimplemented-error> :method-name ',name)))
       (export ',name))))

(define-dbi-interface make-connection (<dbi-driver> &rest connection-args)
  "Create a instance of `<dbi-connection>` for the `driver`.
This method must be implemented in each drivers.

CONNECTION-ARGS will be passed to the underlying driver.")

(define-dbi-interface disconnect (<dbi-connection>)
  "Disconnect the conneciton. Free any resources used by <dbi-connection>.")

@export
(defun find-driver (driver-name)
  "Find a driver class named as `driver-name`.
`driver-name` is a string designer.
Driver should be named like '<DBD-SOMETHING>' for a database 'something'."
  (find-if
   (lambda (class)
     (string= (format nil "<DBD-~:@(~A~)>" driver-name)
              (class-name class)))
   (list-all-drivers)))

@export
(defun list-all-drivers ()
  "Return a list of direct subclasses for `<dbi-driver>`."
  (c2mop:class-direct-subclasses (find-class '<dbi-driver>)))

@export
(defclass <dbi-query> ()
     ((connection :type <dbi-connection>
                  :initarg :connection
                  :initform nil
                  :accessor query-connection)
      (prepared :type t
                :initarg :prepared
                :accessor query-prepared))
  (:documentation "Class that represents a prepared DB query."))

@export
(defclass <dbi-query-result-set> ()
  ((query :initarg :query :accessor result-set-query)))

(define-dbi-interface result-set-column-names (<dbi-query-result-set>)
  "Names, according to the underlying database, of the columns in the result set.")

@export
(defgeneric prepare (<dbi-connection> sql &key &allow-other-keys)
  (:documentation "Preparing executing SQL statement and returns a instance of `<dbi-query>`.
This method may be overrided by subclasses.")
  (:method ((conn <dbi-connection>) (sql string) &key &allow-other-keys)
    (make-instance '<dbi-query>
                   :connection conn
                   :prepared (prepare-sql conn sql))))

(define-dbi-interface fetch-next (<dbi-query-result-set>)
  "Fetch the next row from `query` which is returned by `execute`.")

@export
(defmethod fetch-all ((query <dbi-query>))
  "Fetch all rest rows from `query`."
  (loop for result = (fetch-next query)
        while result
        collect result))

(define-dbi-interface fetch-using-connection (<dbi-connection> <dbi-query>)
  "Fetch the next row of <DBI-QUERY>. Returns NIL if no more rows available.")

@export
(defmethod do-sql ((conn <dbi-connection>) (sql string) &rest params)
  "Do preparation and execution at once.
This method may be overrided by subclasses."
  (apply #'execute (prepare conn sql) params)
  nil)

(define-dbi-interface execute (<dbi-query> params)
  "Execute `query`with query parameters bound to the values PARAMS.")

(define-dbi-interface begin-transaction (<dbi-connection>)
  "Start a transaction.")

(defmethod begin-transaction :around ((conn <dbi-connection>))
  "Turn `auto-commit` off automatically before starting a transaction."
  (symbol-macrolet ((auto-commit (slot-value conn 'auto-commit)))
     (let ((saved auto-commit))
       (setf auto-commit nil)
       (unwind-protect (call-next-method)
         (setf auto-commit saved)))))

(define-dbi-interface commit (<dbi-connection>)
  "Commit changes and end the current transaction.")

@export
(define-dbi-interface rollback (<dbi-connection>)
  "Rollback all changes and end the current transaction.")

@export
(defmethod escape-sql ((conn <dbi-connection>) (sql string))
  "Return escaped `sql`.
This method may be overrided by subclasses when needed.
For example, in case of MySQL and PostgreSQL, backslashes must be escaped by doubling it."
  (with-output-to-string (out)
    (loop for c across sql
          if (char= c #\')
            do (write-sequence "''" out)
          else
            do (write-char c out))))

(defmethod prepare-sql ((conn <dbi-connection>) (sql string))
  "Create a function that takes parameters, binds them into a query and returns SQL as a string."
  (labels ((param-to-sql (param)
             (typecase param
               (string (concatenate 'string "'" (escape-sql conn param) "'"))
               (null "NULL")
               (t (princ-to-string param)))))
    (let ((sql-parts (split-sequence #\? sql)))
      (lambda (&rest params)
        (if params
            (with-output-to-string (out)
              (loop for part in sql-parts
                    do
                 (let ((param (pop params)))
                   (write-sequence
                    (if param
                        (concatenate 'string part (param-to-sql param))
                        part)
                    out))))
            sql)))))
