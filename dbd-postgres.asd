#|
  This file is a part of CL-DBI project.
  Copyright (c) 2011 Eitaro Fukamachi (e.arrows@gmail.com)
|#

#|
  Database driver for PostgreSQL.

  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage dbd-postgres-asd
  (:use :cl :asdf))
(in-package :dbd-postgres-asd)

(defsystem dbd-postgres
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:dbi
               :cl-postgres
               :trivial-garbage
               :cl-syntax
               :cl-syntax-annot)
  :components ((:module "src/dbd"
                :components
                ((:file "postgres"))))
  :description "Database driver for PostgreSQL.")
