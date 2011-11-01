#|
  This file is a part of CL-DBI project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage dbi.driver
  (:use :cl)
  (:import-from :cl-ppcre
                :regex-replace
                :regex-replace-all)
  (:import-from :c2mop
                :class-direct-subclasses)
  (:import-from :dbi.error
                :<dbi-error>))
(in-package :dbi.driver)

(cl-syntax:use-syntax :annot)

@export
(defclass <dbi-driver> ()
     ((driver-name :type (or string keyword)
                   :initarg :driver-name
                   :initform (error "A slot `name` is required for `<dbi-driver>`.")))
  (:documentation "Base class for DB driver."))

@export
(defclass <dbi-connection> ()
     ((driver-name :type string
                   :initarg :driver-name
                   :initform (error "A slot `driver-name` is required for `<dbi-connection>`."))
      (openp :type boolean
             :initarg :openp
             :initform t))
  (:documentation "Base class for managing DB connection."))

@export
(defclass <dbi-query> ()
     ((connection :type <dbi-connection>
                  :initarg :connection
                  :initform nil
                  :accessor connection)
      (prepared :type function
                :initarg :prepared
                :accessor prepared))
  (:documentation "Class that represents a prepared DB query."))

@export
(defmethod make-connection ((driver <dbi-driver>) params)
  (declare (ignore driver params))
  (error '<dbi-error>
         :format-control "`make-connection' should be implemented in a subclass of `<dbi-driver>'."))

@export
(defun list-all-drivers ()
  (c2mop:class-direct-subclasses (find-class '<dbi-driver>)))

@export
(defun find-driver (driver-name)
  (find-if
   (lambda (class)
     (string= (format nil "<DBD-~:(~A~)>" driver-name)
              (class-name class)))
   (list-all-drivers)))

@export
(defmethod execute-using-connection ((conn <dbi-connection>) (query <dbi-query>) params)
  (declare (ignore conn query params))
  (error '<dbi-error>
         :format-control "`execute-using-connection' should be implemented."))

@export
(defmethod prepare ((conn <dbi-connection>) (sql string))
  "Preparing executing SQL statement. Returns `<dbi-query>` instance."
  (make-instance '<dbi-query>
     :connection conn
     :prepared (prepare-sql conn sql)))

@export
(defmethod execute ((query <dbi-query>) &rest params)
  (execute-using-connection
   (connection query)
   query
   params))

@export
(defmethod prepare-sql ((conn <dbi-connection>) (sql string))
  "Create a function that takes parameters, binds them into a query and returns SQL as a string."
  ;; TODO: improve efficiency.
  (lambda (&rest params)
    (reduce (lambda (sql v)
              (ppcre:regex-replace "\\?" sql v))
            (mapcar
             (lambda (param)
               (typecase param
                 (string (concatenate 'string "'" (escape-sql param) "'"))
                 (null "NULL")
                 (t (princ-to-string param))))
             params)
            :initial-value sql)))

@export
(defmethod escape-sql ((conn <dbi-connection>) (sql string))
  (ppcre:regex-replace-all "'" sql  "''"))
