(defsystem "dbi"
  :version "0.9.2"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on ("split-sequence"
               "closer-mop"
               "bordeaux-threads")
  :components ((:module "src"
                :components
                ((:file "dbi" :depends-on ("driver" "logger"))
                 (:file "driver" :depends-on ("error"))
                 (:file "logger")
                 (:file "error"))))
  :description "Database independent interface for Common Lisp"
  :in-order-to ((test-op (test-op "dbi/test"))))

(defsystem "dbi/test"
  :depends-on ("dbi"
               "dbi-test"
               "dbd-sqlite3"
               "dbd-mysql"
               "dbd-postgres"
               "rove"
               "closer-mop"
               "alexandria"
               "trivial-types")
  :components ((:module "tests"
                :pathname "t"
                :components
                ((:file "driver")
                 (:module "dbd"
                  :components
                  ((:file "sqlite3")
                   (:file "postgres")
                   (:file "mysql"))))))
  :perform (test-op (op c) (symbol-call '#:rove '#:run c)))
