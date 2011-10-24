#|
  This file is a part of CL-DBI project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage dbi.driver
  (:use :cl)
  (:import-from :dbi.connection
                :prepare))
(in-package :dbi.driver)

(cl-syntax:use-syntax :annot)

@export
(defclass <dbi-driver> ()
     ((name :type (or string keyword)
            :initarg :name
            :initform (error "A slot `name` is required for `<dbi-driver>`.")))
  (:documentation "Base class for DB driver."))

@export
(defmethod make-connection ((this <dbi-driver>) params &key name password)
  (declare (ignore params name password))
  (error "`make-connection' should be implemented in a subclass of `<dbi-driver>'."))
