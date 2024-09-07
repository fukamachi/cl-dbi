(uiop:define-package #:dbi
  (:nicknames #:cl-dbi)
  (:use #:cl
        #:dbi.cache)
  (:use-reexport #:dbi.error
                 #:dbi.driver
                 #:dbi.logger)
  (:export #:connect
           #:connect-cached
           #:disconnect-cached-all
           #:with-connection))
(in-package #:dbi)

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

(defvar *threads-connection-pool* (make-cache-pool :cleanup-fn #'disconnect))

(defparameter *connection-cache-seconds*
  (* 60 60 24 internal-time-units-per-second))

(defun connect-cached (&rest connect-args)
  (let* ((pool *threads-connection-pool*)
         (conn (get-object pool connect-args)))
    (if (and conn
             (ping conn))
        (progn
          (when (< (+ (connection-established-at conn) *connection-cache-seconds*)
                   (get-internal-real-time))
            (disconnect conn)
            (setf conn (apply #'connect connect-args))
            (setf (get-object pool connect-args) conn))
          conn)
        (prog1
            (setf (get-object pool connect-args)
                  (apply #'connect connect-args))
          (cleanup-cache-pool pool)))))

(defun disconnect-cached-all ()
  (let ((pool *threads-connection-pool*))
    (cleanup-cache-pool pool :force t))
  (values))

(defmacro with-autoload-on-missing (&body body)
  (let ((retrying (gensym))
        (e (gensym)))
    `(let ((,retrying (make-hash-table :test 'equal)))
       (handler-bind ((asdf:missing-component
                        (lambda (,e)
                          (unless (gethash (asdf::missing-requires ,e) ,retrying)
                            (setf (gethash (asdf::missing-requires ,e) ,retrying) t)
                            (when (find :quicklisp *features*)
                              (uiop:symbol-call '#:ql-dist '#:ensure-installed
                                                (uiop:symbol-call '#:ql-dist '#:find-system
                                                                  (asdf::missing-requires ,e)))
                              (invoke-restart (find-restart 'asdf:retry ,e)))))))
         ,@body))))

(defun load-driver (driver-name)
  (let ((driver-system (intern (format nil "DBD-~A" driver-name) :keyword)))
    (with-autoload-on-missing
      (let ((*standard-output* (make-broadcast-stream))
            (*error-output* (make-broadcast-stream)))
        (asdf:load-system driver-system :verbose nil)))))


(defmacro with-connection ((conn-sym &rest rest) &body body)
  `(let ((,conn-sym (connect ,@rest)))
     (unwind-protect
          (progn ,@body)
       (disconnect ,conn-sym))))
