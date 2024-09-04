(defsystem "dbd-postgres"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on ("dbi"
               "cl-postgres"
               "trivial-garbage")
  :components ((:module "src/dbd"
                :components
                ((:file "postgres"))))
  :description "Database driver for PostgreSQL.")
