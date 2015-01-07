#!/bin/sbci --script
(load "quicklisp.lisp")
(quicklisp-quickstart:install)

(ql:quickload :prove)
(ql:quickload :dbi)
(ql:quickload :dbi-test)

(if (and (prove:run #P"t/driver.lisp")
         (prove:run #P"t/dbd/mysql.lisp")
         (prove:run #P"t/dbd/postgres.lisp")
         (prove:run #P"t/dbd/sqlite3.lisp"))
    (sb-ext:quit :unix-status 0)
    (sb-ext:quit :unix-status 1))
