#|
  This file is a part of CL-DBI project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage dbi
  (:use :cl)
  (:import-from :dbi.connection
                :prepare
                :execute)
  (:export :prepare
           :execute))
(in-package :dbi)

(cl-syntax:use-syntax :annot)

@export
(defun connect (dsn &key username password)
  ;; TODO
  )
