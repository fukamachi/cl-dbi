#|
  This file is a part of CL-DBI project.
  Copyright (c) 2011 Eitaro Fukamachi (e.arrows@gmail.com)
|#

#|
  Database independent interface for Common Lisp

  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage dbi-asd
  (:use :cl :asdf))
(in-package :dbi-asd)

(defsystem dbi
  :version "0.9.2"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:cl-syntax
               :cl-syntax-annot
               :split-sequence
               :closer-mop
               :bordeaux-threads)
  :components ((:module "src"
                :components
                ((:file "dbi" :depends-on ("driver" "logger"))
                 (:file "driver" :depends-on ("error"))
                 (:file "logger")
                 (:file "error"))))
  :description "Database independent interface for Common Lisp"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op dbi-test))))
