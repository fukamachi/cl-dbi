#|
  This file is a part of CL-DBI project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage dbi.error
  (:use :cl))
(in-package :dbi.error)

(cl-syntax:use-syntax :annot)

@export
(define-condition <dbi-error> (simple-error) ())

@export
(define-condition <dbi-interface-error> (<dbi-error>) ())

@export
(define-condition <dbi-unimplemented-error> (<dbi-interface-error>)
  ((method-name :type (or symbol string)))
  (:report
   (lambda (condition stream)
     (format stream
             "`~A' must be implemented."
             (slot-value condition 'method-name)))))

@export
(define-condition <dbi-database-error> (<dbi-error>) ())

@export
(define-condition <dbi-operation-error> (<dbi-database-error>) ())

@export
(define-condition <dbi-integrity-error> (<dbi-database-error>) ())

@export
(define-condition <dbi-internal-error> (<dbi-database-error>) ())

@export
(define-condition <dbi-programming-error> (<dbi-database-error>) ())

@export
(define-condition <dbi-notsupported-error> (<dbi-database-error>) ())
