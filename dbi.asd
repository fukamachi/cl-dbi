(defsystem "dbi"
  :version "0.9.2"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on ("cl-syntax"
               "cl-syntax-annot"
               "split-sequence"
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
               "rove"
               "closer-mop"
               "cl-syntax"
               "cl-syntax-annot"
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
