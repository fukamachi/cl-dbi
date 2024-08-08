(defsystem "dbi"
  :version "0.11.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on ("split-sequence"
               "closer-mop"
               "cl-ppcre"
               (:feature #1=(:or :abcl
                                 (:and :allegro :multiprocessing)
                                 (:and :clasp :threads)
                                 (:and :clisp :mt)
                                 (:and :ccl :openmcl-native-threads)
                                 (:and :cmu :mp)
                                 :corman
                                 (:and :ecl :threads)
                                 :mkcl
                                 :lispworks
                                 (:and :sbcl :sb-thread)
                                 :scl)
                         "bordeaux-threads"))
  :components ((:module "src"
                :depends-on ("src/utils")
                :components
                ((:file "dbi" :depends-on ("driver" "cache" "logger"))
                 (:file "driver" :depends-on ("error"))
                 (:module "cache"
                  :components
                  ((:file "thread" :if-feature #1#)
                   (:file "single" :if-feature (:not #1#))))
                 (:file "logger")
                 (:file "error")))
               (:file "src/utils"))
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
