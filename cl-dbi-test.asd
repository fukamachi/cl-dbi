#|
  This file is a part of CL-DBI project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-dbi-test-asd
  (:use :cl :asdf))
(in-package :cl-dbi-test-asd)

(defsystem cl-dbi-test
  :author "Eitarow Fukamachi"
  :license "LLGPL"
  :depends-on (:cl-dbi
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "cl-dbi"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
