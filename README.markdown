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

## Description

CL-DBI is intended to provide the same interface for each database.

Not only, you don't have to learn each API of databases anymore, but this layer is especially convenient when you want to use the different database by environment.

For example, your application had better to use efficient database such as MySQL on the production environment, but you may want to use SQLite3 on your machine. In that case, all what you have to do is only to rewrite calling `dbi:connect`.

## Installation

This library will be available on Quicklisp when ready to use.

## API

### User-Level API

* connect [driver-name &amp; params] =&gt; &lt;dbi-connection&gt;
* prepare [conn sql] =&gt; &lt;dbd-query&gt;
* execute [query &amp; params] =&gt; something
* do-sql [conn sql &amp; params] =&gt; something
* list-all-drivers [] =&gt; (&lt;dbi-driver&gt; ..)
* find-driver [driver-name] =&gt; &lt;dbi-driver&gt;

### Driver-Level API

* &lt;dbi-driver&gt;
* &lt;dbi-connection&gt;
* make-connection [driver params]
* prepare [conn sql] =&gt; &lt;dbd-query&gt;
* do-sql [conn sql &amp; params] =&gt; something
* execute-using-connection =&gt; something
* escape-sql =&gt; string

## Creating a new driver

Create a subclass of &lt;dbi-driver&gt; and implement following methods.

* make-connection
* execute-using-connection

And these methods may be overrided if needed.

* prepare
* do-sql
* escape-sql

## Dependencies

* cl-annot
* CL-Syntax
* SPLIT-SEQUENCE
* closer-mop

## Author

* Eitarow Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)

# License

Licensed under the LLGPL License.

