#|
  This file is a part of CL-DBI project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage dbi
  (:use :cl)
  (:nicknames :cl-dbi)
  (:import-from :dbi.driver
                :list-all-drivers
                :find-driver
                :make-connection
                :prepare
                :execute
                :fetch
                :do-sql)
  (:export :list-all-drivers
           :find-driver
           :prepare
           :execute
           :fetch
           :do-sql))
(in-package :dbi)

(cl-syntax:use-syntax :annot)

@export
(defun connect (driver-name &rest params &key database-name &allow-other-keys)
  "Open a connection to the database which corresponds to `driver-name`."
  (let ((driver (find-driver driver-name)))
    (unless driver
      (load-driver driver-name)
      (setf driver (find-driver driver-name)))

    (unless driver
      (error 'simple-error
             :format-control "Driver ~A is not found."
             :format-arguments driver-name))

    (apply #'make-connection (make-instance driver) params)))

(defun load-driver (driver-name)
  (let ((driver-system (intern (format nil "DBD-~A" driver-name) :keyword)))
    #+quicklisp (ql:quickload driver-system :verbose nil)
    #-quicklisp
    (asdf:load-system driver-system :verbose nil)))
