(defsystem "dbd-sqlite3"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on ("dbi"
               "sqlite"
               "trivial-garbage")
  :components ((:module "src/dbd"
                :components
                ((:file "sqlite3"))))
  :description "Database driver for SQLite3.")
