#|
  This file is a part of CL-DBI project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage dbi.driver
  (:use :cl
        :split-sequence)
  (:import-from :c2mop
                :class-direct-subclasses)
  (:import-from :dbi.error
                :<dbi-unimplemented-error>)
  (:export :query-connection
           :query-sql
           :query-prepared))
(in-package :dbi.driver)

(cl-syntax:use-syntax :annot)

(declaim (optimize (speed 3)))

@export
(defclass <dbi-driver> () ()
  (:documentation "Base class for DB driver."))

@export
(defclass <dbi-connection> () ()
  (:documentation "Base class for managing DB connection."))

@export
(defmethod make-connection ((driver <dbi-driver>) &key)
  "Create a instance of `<dbi-connection>` for the `driver`.
This method must be implemented in each drivers."
  (declare (ignore driver))
  (error '<dbi-unimplemented-error>
         :method-name 'make-connection))

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
      (sql :type string
           :initarg :sql
           :accessor query-sql)
      (prepared :type t
                :initarg :prepared
                :accessor query-prepared))
  (:documentation "Class that represents a prepared DB query."))

(defmethod initialize-instance :after ((query <dbi-query>) &key)
  (with-slots (connection sql prepared) query
     (setf prepared
           (prepare-sql connection sql))))

@export
(defmethod prepare ((conn <dbi-connection>) (sql string) &key (query-class '<dbi-query>))
  "Preparing executing SQL statement and returns a instance of `<dbi-query>`.
This method may be overrided by subclasses."
  (make-instance query-class
     :connection conn
     :sql sql))

@export
(defmethod execute ((query <dbi-query>) &rest params)
  "Execute `query` with `params` and return the results."
  (execute-using-connection
   (query-connection query)
   query
   params))

@export
(defmethod fetch ()
  ;; TODO
  )

@export
(defmethod do-sql ((conn <dbi-connection>) (sql string) &rest params)
  "Do preparation and execution at once.
This method may be overrided by subclasses."
  (apply #'execute (prepare conn sql) params))

@export
(defmethod execute-using-connection ((conn <dbi-connection>) (query <dbi-query>) params)
  "Execute `query` in `conn`.
This method must be implemented in each drivers."
  (declare (ignore conn query params))
  (error '<dbi-unimplemented-error>
         :method-name 'execute-using-connection))

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
        (with-output-to-string (out)
          (loop for part in sql-parts
                for param in params
                do (write-sequence
                    (concatenate 'string
                                 part
                                 (param-to-sql param))
                         out)))))))
