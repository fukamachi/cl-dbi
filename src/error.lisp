#|
  This file is a part of CL-DBI project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage dbi.error
  (:use :cl))
(in-package :dbi.error)

(cl-syntax:use-syntax :annot)

@export
(defclass <dbi-error> (simple-error) ())
