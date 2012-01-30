#|
  This file is a part of CL-DBI project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage dbi-test-asd
  (:use :cl :asdf))
(in-package :dbi-test-asd)

(defsystem dbi-test
  :author "Eitarow Fukamachi"
  :license "LLGPL"
  :depends-on (:dbi
               :cl-test-more
               :closer-mop
               :cl-syntax
               :cl-syntax-annot
               :trivial-types)
  :components ((:module "src"
                :components
                ((:file "test")))
               (:module "t"
                :depends-on ("src")
                :components
                ((:file "driver")
                 (:module "dbd"
                  :components
                  ((:file "sqlite3")
                   (:file "postgres")
                   (:file "mysql"))))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
