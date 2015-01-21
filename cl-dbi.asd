(in-package :cl-user)
(defpackage cl-dbi-asd
  (:use :cl :asdf))
(in-package :cl-dbi-asd)

(defsystem cl-dbi
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:dbi))
