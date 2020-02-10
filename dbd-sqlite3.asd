(defsystem "dbd-sqlite3"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on ("dbi"
               "sqlite"
               "trivial-garbage")
  :components ((:module "src/dbd"
                :components
                ((:file "sqlite3"))))
  :description "Database driver for SQLite3.")
