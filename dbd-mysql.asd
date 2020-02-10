(defsystem "dbd-mysql"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on ("dbi"
               "cl-mysql")
  :components ((:module "src/dbd"
                :components
                ((:file "mysql" :depends-on ("mysql/error"))
                 (:file "mysql/error"))))
  :description "Database driver for MySQL.")
