(defsystem "dbd-null"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on ("dbi")
  :components ((:module "src/dbd"
                :components
                ((:file "null"))))
  :description "Database null driver for unit testing.")
