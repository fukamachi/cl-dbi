(defsystem "dbd-postgres"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on ("dbi"
               "cl-postgres"
               "trivial-garbage")
  :components ((:module "src/dbd"
                :components
                ((:file "postgres"))))
  :description "Database driver for PostgreSQL.")
