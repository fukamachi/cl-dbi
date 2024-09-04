(defsystem "dbd-mysql"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on ("dbi"
               "cl-mysql")
  :components ((:module "src/dbd"
                :components
                ((:file "mysql" :depends-on ("mysql/error"))
                 (:file "mysql/error"))))
  :description "Database driver for MySQL.")
