(defpackage #:dbi.error
  (:use #:cl
        #:dbi.utils)
  (:export #:dbi-error
           #:dbi-warning
           #:dbi-interface-error
           #:dbi-unimplemented-error
           #:dbi-database-error
           #:dbi-data-error
           #:dbi-operational-error
           #:dbi-integrity-error
           #:dbi-internal-error
           #:dbi-programming-error
           #:dbi-notsupported-error
           #:dbi-already-commited-error
           #:dbi-already-rolled-back-error
           #:<dbi-error>
           #:<dbi-warning>
           #:<dbi-interface-error>
           #:<dbi-unimplemented-error>
           #:<dbi-database-error>
           #:<dbi-data-error>
           #:<dbi-operational-error>
           #:<dbi-integrity-error>
           #:<dbi-internal-error>
           #:<dbi-programming-error>
           #:<dbi-notsupported-error>
           #:<dbi-already-commited-error>
           #:<dbi-already-rolled-back-error>
           #:database-error-message
           #:database-error-code))
(in-package #:dbi.error)

(define-condition/a dbi-error (simple-error) ()
  (:documentation "Base class of all other error exceptions. Use this to catch all DBI
errors."))

(define-condition/a dbi-warning (simple-warning) ()
  (:documentation "For important warnings like data truncation, etc."))

(define-condition/a dbi-interface-error (dbi-error) ()
  (:documentation "Exception for errors related to the DBI interface rather than the
database itself."))

(define-condition/a dbi-unimplemented-error (dbi-interface-error)
  ((method-name :initarg :method-name :type (or symbol string)))
  (:documentation "Exception raised if the DBD driver has not specified a mandatory method.")
  (:report
   (lambda (condition stream)
     (format stream
             "`~A' must be implemented."
             (slot-value condition 'method-name)))))

(define-condition/a dbi-database-error (dbi-error)
  ((message :initarg :message
            :reader database-error-message)
   (error-code :initarg :error-code
               :reader database-error-code))
  (:documentation "Exception for errors related to the database.")
  (:report
   (lambda (condition stream)
     (format stream
             "DB Error: ~A (Code: ~A)"
             (slot-value condition 'message)
             (slot-value condition 'error-code)))))

(define-condition/a dbi-data-error (dbi-database-error) ()
  (:documentation "Exception for errors due to problems with the processed
data such as division for zero, numeric value out of range, etc."))

(define-condition/a dbi-operational-error (dbi-database-error) ()
  (:documentation "Exception for errors related to the database's operation which are not
necessarily under the control of the programmer.  This includes such
things as unexpected disconnect, datasource name not found, transaction
could not be processed, a memory allocation error occured during
processing, etc."))

(define-condition/a dbi-integrity-error (dbi-database-error) ()
  (:documentation "Exception raised when the relational integrity of the database
is affected, e.g. a foreign key check fails."))

(define-condition/a dbi-internal-error (dbi-database-error) ()
  (:documentation "Exception raised when the database encounters an internal error,
e.g. the cursor is not valid anymore, the transaction is out of sync."))

(define-condition/a dbi-programming-error (dbi-database-error) ()
  (:documentation "Exception raised for programming errors, e.g. table not found
or already exists, syntax error in SQL statement, wrong number
of parameters specified, etc."))

(define-condition/a dbi-notsupported-error (dbi-database-error)
  ((method-name :initarg :method-name :type (or symbol string)))
  (:documentation "Exception raised if e.g. commit() is called for a database which do not
support transactions.")
  (:report
   (lambda (condition stream)
     (format stream
             "`~A' isn't supported on the current driver."
             (slot-value condition 'method-name)))))

(define-condition/a dbi-already-commited-error (dbi-error) ()
  (:documentation "This exception occur when there is a programming error and
you are trying to commit a transaction twice."))

(define-condition/a dbi-already-rolled-back-error (dbi-error) ()
  (:documentation "This exception occur when there is a programming error and
you are trying to rollback a transaction twice."))
