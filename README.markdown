# CL-DBI - Database independent interface for Common Lisp

## Usage

    (defvar *connection*
        (dbi:connect :mysql
                     :database-name "test"
                     :username "nobody"
                     :password "1234"))

    (let ((query (dbi:prepare *connection*
                   "SELECT * FROM somewhere WHERE flag = ? OR updated_at > ?")))
      (dbi:execute query 0 "2011-11-01"))

## Installation

## API

### User-Level API

* connect [driver-name &amp; params] =&gt; &lt;dbi-connection&gt;
* prepare [conn sql] =&gt; &lt;dbi-query&gt;
* execute [query &amp; params] =&gt; something
* list-all-drivers [] =&gt; (&lt;dbi-driver&gt; ..)
* find-driver [driver-name] =&gt; &lt;dbi-driver&gt;

### Driver-Level API

* &lt;dbi-driver&gt;
* &lt;dbi-connection&gt;
* &lt;dbi-query&gt;
* make-connection [driver params]
* prepare [conn sql] =&gt; &lt;dbi-query&gt;
* prepare-sql [conn sql] =&gt; function
* execute-using-connection =&gt; something
* escape-sql =&gt; string

## Creating a new driver

Create a subclass of &lt;dbi-driver&gt; and implement following methods.

* make-connection
* execute-using-connection

## Dependencies

* cl-annot
* CL-Syntax
* CL-PPCRE
* closer-mop

## Author

* Eitarow Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)

# License

Licensed under the LLGPL License.

