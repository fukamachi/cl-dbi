#|
  This file is a part of CL-DBI project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage dbi.driver
  (:use :cl)
  (:import-from :c2mop
                :class-direct-subclasses))
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
(defmethod make-connection ((this <dbi-driver>) params)
  (declare (ignore params))
  (error "`make-connection' should be implemented in a subclass of `<dbi-driver>'."))

@export
(defun find-driver (driver-name)
  (find-if
   (lambda (class)
     (or (string= driver-name (class-name class))
         (string= (format nil "<DBI-DRIVER-~:(~A~)>" driver-name)
                  (class-name class))))
   (c2mop:class-direct-subclasses (find-class '<dbi-driver>))))

@export
(defmethod execute-using-connection ((conn <dbi-connection>) (query <dbi-query>) params)
  (declare (ignore params))
  (error "`execute-using-connection' should be implemented."))

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

(defmethod prepare-sql ((conn <dbi-connection>) (sql string))
  "Create a function that takes parameters, binds them into a query and returns SQL as a string."
  ;; TODO
  )
