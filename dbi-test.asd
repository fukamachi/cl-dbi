(defsystem "dbi-test"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on ("dbi"
               "rove"
               "alexandria"
               "trivial-types")
  :components ((:module "src"
                :components
                ((:file "test")))))
