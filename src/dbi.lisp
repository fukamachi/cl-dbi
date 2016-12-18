(in-package :cl-user)
(defpackage dbi
  (:use :cl
        :dbi.error)
  (:nicknames :cl-dbi)
  (:import-from :dbi.driver
                :list-all-drivers
                :find-driver
                :connection-driver-type
                :connection-database-name
                :make-connection
                :disconnect
                :prepare
                :execute
                :fetch
                :fetch-all
                :do-sql
                :begin-transaction
                :commit
                :rollback
                :ping
                :row-count)
  (:import-from :bordeaux-threads
                :current-thread
                :thread-alive-p)
  (:export :list-all-drivers
           :find-driver
           :connection-driver-type
           :connection-database-name
           :disconnect
           :prepare
           :execute
           :fetch
           :fetch-all
           :do-sql
           :begin-transaction
           :commit
           :rollback
           :ping
           :row-count

           :<dbi-error>
           :<dbi-warning>
           :<dbi-interface-error>
           :<dbi-unimplemented-error>
           :<dbi-database-error>
           :<dbi-data-error>
           :<dbi-operational-error>
           :<dbi-integrity-error>
           :<dbi-internal-error>
           :<dbi-programming-error>
           :<dbi-notsupported-error>))
(in-package :dbi)

(cl-syntax:use-syntax :annot)

@export
(defun connect (driver-name &rest params &key database-name &allow-other-keys)
  "Open a connection to the database which corresponds to `driver-name`."
  (declare (ignore database-name))
  (let ((driver (find-driver driver-name)))
    (unless driver
      (load-driver driver-name)
      (setf driver (find-driver driver-name)))

    (unless driver
      (error 'simple-error
             :format-control "Driver ~A is not found."
             :format-arguments driver-name))

    (apply #'make-connection (make-instance driver) params)))

(defun make-connection-pool ()
  (make-hash-table :test 'equal))

#+thread-support
(defun make-threads-connection-pool ()
  (let ((pool (make-hash-table :test 'eq)))
    (setf (gethash (bt:current-thread) pool) (make-connection-pool))
    pool))
#-thread-support
(defun make-threads-connection-pool ()
  (make-connection-pool))

(defvar *threads-connection-pool* (make-threads-connection-pool))

(defun get-connection-pool ()
  (or (gethash (bt:current-thread) *threads-connection-pool*)
      (setf (gethash (bt:current-thread) *threads-connection-pool*)
            (make-connection-pool))))

@export
(defun connect-cached (&rest connect-args)
  (let* ((pool (get-connection-pool))
         (conn (gethash connect-args pool)))
    (cond
      ((null conn)
       (cleanup-connection-pool)
       (setf (gethash connect-args pool)
             (apply #'connect connect-args)))
      ((not (ping conn))
       (disconnect conn)
       (remhash connect-args pool)
       (cleanup-connection-pool)
       (setf (gethash connect-args pool)
             (apply #'connect connect-args)))
      (t conn))))

(defun cleanup-connection-pool ()
  (maphash (lambda (thread pool)
             (unless (bt:thread-alive-p thread)
               (maphash (lambda (args conn)
                          (declare (ignore args))
                          (disconnect conn))
                        pool)
               (remhash thread *threads-connection-pool*)))
           *threads-connection-pool*))

(defun load-driver (driver-name)
  (let ((driver-system (intern (format nil "DBD-~A" driver-name) :keyword)))
    #+quicklisp (ql:quickload driver-system :verbose nil :silent t)
    #-quicklisp
    (asdf:load-system driver-system :verbose nil)))

@export
(defmacro with-transaction (conn &body body)
  "Start a transaction and commit at the end of this block. If the evaluation `body` is interrupted, the transaction is rolled back automatically."
  (let ((ok (gensym "TRANSACTION-OK"))
        (conn-var (gensym "CONN-VAR")))
    `(let (,ok
           (,conn-var ,conn))
       (begin-transaction ,conn-var)
       (unwind-protect (multiple-value-prog1
                         (progn ,@body)
                         (setf ,ok t))
         (if ,ok
             (commit ,conn-var)
             (rollback ,conn-var))))))

@export
(defmacro with-connection ((conn-sym &rest rest) &body body)
  `(let ((,conn-sym (connect ,@rest)))
     (unwind-protect
          (progn ,@body)
       (disconnect ,conn-sym))))
