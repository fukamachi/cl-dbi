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
                :execute)
  (:export :list-all-drivers
           :find-driver
           :prepare
           :execute))
(in-package :dbi)

(cl-syntax:use-syntax :annot)

@export
(defun connect (driver-name &rest params &allow-other-keys)
  (let ((driver (find-driver driver-name)))
    (unless driver
      (error 'simple-error
             :format-control "Driver ~A is not found."
             :format-arguments driver-name))
    (apply #'make-connection driver params)))

@export
(defun disconnect (conn)
  ;; TODO
  )
