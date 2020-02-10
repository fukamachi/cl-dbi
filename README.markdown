# CL-DBI - Database independent interface for Common Lisp

[![Build Status](https://github.com/fukamachi/cl-dbi/workflows/CI/badge.svg)](https://github.com/fukamachi/cl-dbi/actions?query=workflow%3ACI)

## Usage

### Connecting and executing a query

```common-lisp
(defvar *connection*
  (dbi:connect :mysql
               :database-name "test"
               :username "nobody"
               :password "1234"))

(let* ((query (dbi:prepare *connection*
                           "SELECT * FROM somewhere WHERE flag = ? OR updated_at > ?"))
       (query (dbi:execute query 0 "2011-11-01")))
  (loop for row = (dbi:fetch result)
        while row
        ;; process "row".
        ))

;; Do it at once
(dbi:fetch-all (dbi:execute (dbi:prepare *connection* "SELECT * FROM somewhere WHERE flag = ? OR updated_at > ?")
                            0 "2011-11-01"))
```

`dbi:do-sql` is another option which prepare and execute a single statement. It returns the number of rows affected. It's typically used for non-`SELECT` statements.

```common-lisp
(dbi:do-sql *connection*
            "INSERT INTO somewhere (flag, updated_at) VALUES (?, NOW())"
            0)
;=> 1
```

### Using `dbi:with-connection` to ensure connections are closed

```common-lisp
(dbi:with-connection (conn :sqlite3 :database-name "/home/fukamachi/test.db")
  (let* ((query (dbi:prepare conn "SELECT * FROM People"))
         (query (dbi:execute query)))
    (loop for row = (dbi:fetch query)
          while row
          do (format t "~A~%" row))))
```

### Connection pooling

`dbi:connect-cached` returns a existing connection if the database is already connected. Since the cache will be created for each thread, it's safe to use in a multithread application.

## Description

CL-DBI provides the same interface for multiple SQL databases. You need not learn the API of each database.

This library is especially convenient when you want to use different databases in different environments. For example, you may use MySQL as a production database, but use SQLite3 on your development system. To switch database backends you need only change the arguments to `dbi:connect`.

## Databases

* SQLite3
* PostgreSQL
* MySQL

## Installation

This library is available on [Quicklisp](https://www.quicklisp.org/).

```common-lisp
CL-USER> (ql:quickload :cl-dbi)
To load "cl-dbi":
  Load 1 ASDF system:
    cl-dbi
; Loading "cl-dbi"

(:CL-DBI)
```

## API

### User-Level API

* connect [driver-name &amp; params] =&gt; &lt;dbi-connection&gt;
* connect-cached [driver-name &amp; params] =&gt; &lt;dbi-connection&gt;
* disconnect [&lt;dbi-connection&gt;] =&gt; T or NIL
* prepare [conn sql] =&gt; &lt;dbi-query&gt;
* execute [query &amp; params] =&gt; something
* fetch [result] =&gt; a row data as plist
* fetch-all [result] =&gt; a list of all row data
* do-sql [conn sql &amp; params]
* list-all-drivers [] =&gt; (&lt;dbi-driver&gt; ..)
* find-driver [driver-name] =&gt; &lt;dbi-driver&gt;
* with-transaction [conn]
* begin-transaction [conn]
* commit [conn]
* rollback [conn]
* ping [conn] =&gt; T or NIL
* row-count [conn] =&gt; a number of rows modified by the last executed INSERT/UPDATE/DELETE
* with-connection [connection-variable-name &body body]

### Driver-Level API

* &lt;dbi-driver&gt;
* &lt;dbi-connection&gt;
* make-connection [driver params]
* disconnect [&lt;dbi-connection&gt;] =&gt; T or NIL
* prepare [conn sql] =&gt; &lt;dbi-query&gt;
* fetch-using-connection [conn result] =&gt; a row data as plist
* do-sql [conn sql &amp; params]
* execute-using-connection =&gt; something
* escape-sql =&gt; string
* begin-transaction [conn]
* commit [conn]
* rollback [conn]
* ping [conn] =&gt; T or NIL
* row-count [conn] =&gt; a number of rows modified by the last executed INSERT/UPDATE/DELETE
* free-query-resources [query] free resources associated with a prepared query (this is required only for sqlite3 driver at the moment)

## Creating a new driver

Create a subclass of &lt;dbi-driver&gt; and implement following methods.

* make-connection
* disconnect [&lt;dbi-connection&gt;] =&gt; T or NIL
* execute-using-connection

These methods can be overriden if needed.

* prepare
* fetch-using-connection
* do-sql
* escape-sql

## Hook of SQL execution

CL-DBI provides `dbi:*sql-execution-hooks*`, a hook to run for each SQL execution, particularly used for logging.

The hook function takes these 4 values:

- SQL (string)
- placeholder parameters (list)
- Row count of the results (integer or null)
- Took time in miliseconds (integer or null)

The row count and its took time could be null if those values are not available for the driver in some reason.

`dbi:simple-sql-logger` is also provided for just printing those values to `*standard-output*`. It can be enabled with the following code:

```common-lisp
(push #'dbi:simple-sql-logger dbi:*sql-execution-hooks*)
```

## Development

### Running all tests in the Docker

This will not require you to install Postgres or Mysql.
All you need is Docker and Docker Compose.

To run all tests, execute this in the shell:

    docker-compose up tests

### Running specific driver's unittests

Running tests with docker-compose does not allow you
to debug code in SLIME or SLY. To do this, you need
to start databases as separate containers and to make
their ports available to the host machine.

Here is how you can start Postgres and Mysql in Docker
and run unittests agains them:

* Start a docker container with the database

  For example, with postgres:

      docker run --rm -ti \
             -e POSTGRES_USER=cl-dbi \
             -e POSTGRES_PASSWORD=cl-dbi \
             -p 5432:5432 \
             postgres:10

  Or with mysql:

      docker run --rm -ti \
             --name cl-dbi \
             -e MYSQL_ROOT_PASSWORD=cl-dbi \
             -p 3306:3306 \
             mysql:8

      docker exec -ti \
             cl-dbi \
             mysql -pcl-dbi \
                   -e 'create database if not exists `cl-dbi`'

* Then in Lisp repl load the unittests:

      (ql:quickload :dbi/test)
      ;; Turn off colors if you are in the Emacs
      (setf rove:*enable-colors* nil)
      ;; Set this to debug failed test
      (setf rove:*debug-on-error* t)

* And start driver's unittests:

  For postgres:

      (dbi.test:run-driver-tests :postgres
                                 :database-name "postgres"
                                 :host "localhost"
                                 :port 5432
                                 :username "cl-dbi"
                                 :password "cl-dbi")

  For mysql:

      ;; Probably you will need to load library manually if
      ;; it was installed using Homebrew:
      (push "/usr/local/opt/mysql-client/lib/" cffi:*foreign-library-directories*)
      (cffi:load-foreign-library "libmysqlclient.20.dylib"
                                 :search-path "/usr/local/opt/mysql-client/lib/")
      (dbi.test:run-driver-tests :mysql
                                 :database-name "cl-dbi"
                                 :host "127.0.0.1"
                                 :port 3306
                                 :username "root"
                                 :password "cl-dbi")

  Also, you can run a single test like this:

      (dbi.test:run-driver-tests :mysql
                                 :database-name "cl-dbi"
                                 :host "127.0.0.1"
                                 :port 3306
                                 :username "root"
                                 :password "cl-dbi"
                                 :test-name 'select-after-commit)

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2011 Eitaro Fukamachi (e.arrows@gmail.com)

# License

Licensed under the LLGPL License.
