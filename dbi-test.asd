#|
  This file is a part of CL-DBI project.
  Copyright (c) 2011 Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage dbi-test-asd
  (:use :cl :asdf))
(in-package :dbi-test-asd)

(defsystem dbi-test
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:dbi
               :prove
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
                ((:test-file "driver")
                 (:module "dbd"
                  :components
                  ((:test-file "sqlite3")
                   (:test-file "postgres")
                   (:test-file "mysql"))))))

  :defsystem-depends-on (:prove)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove) c)))
