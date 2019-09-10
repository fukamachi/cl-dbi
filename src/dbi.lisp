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
                :savepoint
                :rollback-savepoint
                :release-savepoint
                :*in-transaction*
                :*current-savepoint*
                :ping
                :row-count
                :transaction-done-condition
                :free-query-resources)
  (:import-from :dbi.logger
                :*sql-execution-hooks*
                :simple-sql-logger)
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
           :savepoint
           :rollback-savepoint
           :release-savepoint
           :ping
           :row-count
           :free-query-resources

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
           :<dbi-notsupported-error>

           ;; logger
           :*sql-execution-hooks*
           :simple-sql-logger))
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

(defvar *connection-pool-cleanup-lock*
  (bt:make-lock "connection-pool-cleanup-lock"))
(defun cleanup-connection-pool ()
  (bt:with-lock-held (*connection-pool-cleanup-lock*)
    (maphash (lambda (thread pool)
               (unless (bt:thread-alive-p thread)
                 (maphash (lambda (args conn)
                            (declare (ignore args))
                            (disconnect conn))
                          pool)
                 (remhash thread *threads-connection-pool*)))
             *threads-connection-pool*)))

(defmacro with-retrying (&body body)
  (let ((retrying (gensym))
        (e (gensym))
        (restart (gensym)))
    `(let ((,retrying (make-hash-table :test 'equal)))
       (handler-bind ((asdf:missing-component
                        (lambda (,e)
                          (unless (gethash (asdf::missing-requires ,e) ,retrying)
                            (let ((,restart (find-restart 'asdf:retry)))
                              (when ,restart
                                (setf (gethash (asdf::missing-requires ,e) ,retrying) t)
                                (asdf:clear-configuration)
                                (invoke-restart ,restart)))))))
         ,@body))))

(defun load-driver (driver-name)
  (let ((driver-system (intern (format nil "DBD-~A" driver-name) :keyword)))
    #+quicklisp
    (with-retrying
      (ql:quickload driver-system :verbose nil :silent t))
    #-quicklisp
    (asdf:load-system driver-system :verbose nil)))

(defun generate-random-savepoint ()
  (format nil "savepoint_~36R" (random (expt 36 #-gcl 8 #+gcl 5))))

@export
(defmacro with-savepoint (conn &body body)
  (let ((done (gensym "SAVEPOINT-DONE"))
        (ok (gensym "SAVEPOINT-OK"))
        (conn-var (gensym "CONN-VAR")))
    `(let* (,done
            ,ok
            (,conn-var ,conn)
            (*current-savepoint* (generate-random-savepoint)))
       (savepoint ,conn-var *current-savepoint*)
       (unwind-protect
            (handler-case (multiple-value-prog1
                              (progn ,@body)
                            (setf ,ok t))
              (transaction-done-condition () (setf ,done t)))
         (unless ,done
           (handler-case
               (if ,ok
                   (release-savepoint ,conn-var)
                   (rollback-savepoint ,conn-var))
             (transaction-done-condition ())))))))

(defmacro %with-transaction (conn &body body)
  (let ((done (gensym "TRANSACTION-DONE"))
        (ok (gensym "TRANSACTION-OK"))
        (conn-var (gensym "CONN-VAR")))
    `(let* (,done
            ,ok
            (,conn-var ,conn)
            (*in-transaction* (cons ,conn-var *in-transaction*)))
       (begin-transaction ,conn-var)
       (unwind-protect
            (handler-case (multiple-value-prog1
                              (progn ,@body)
                            (setf ,ok t))
              (transaction-done-condition () (setf ,done t)))
         (unless ,done
           (handler-case
               (if ,ok
                   (commit ,conn-var)
                   (rollback ,conn-var))
             (transaction-done-condition ())))))))

@export
(defmacro with-transaction (conn &body body)
  "Start a transaction and commit at the end of this block. If the evaluation `body` is interrupted, the transaction is rolled back automatically."
  (let ((conn-var (gensym "CONN-VAR")))
    `(let ((,conn-var ,conn))
       (if (find ,conn-var *in-transaction* :test #'eq)
           (with-savepoint ,conn-var ,@body)
           (%with-transaction ,conn-var ,@body)))))

@export
(defmacro with-connection ((conn-sym &rest rest) &body body)
  `(let ((,conn-sym (connect ,@rest)))
     (unwind-protect
          (progn ,@body)
       (disconnect ,conn-sym))))
