(defsystem "dbi-test"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on ("dbi"
               "rove"
               "alexandria"
               "trivial-types")
  :components ((:module "src"
                :components
                ((:file "test")))))
