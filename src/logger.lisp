(defpackage #:dbi.logger
  (:use #:cl)
  (:export #:*sql-execution-hooks*
           #:simple-sql-logger
           #:sql-log
           #:with-took-usec))
(in-package #:dbi.logger)

(defvar *sql-execution-hooks* '())

(defun simple-sql-logger (sql params row-count took-usec)
  (format t "~&~<;; ~@;~A (~{~S~^, ~}) ~@[[~D row~:P]~]~@[ (~Fms)~]~:>~%"
          (list sql
                (mapcar (lambda (param)
                          (if (typep param '(simple-array (unsigned-byte 8) (*)))
                              (map 'string #'code-char param)
                              param))
                        params)
                row-count
                (/ took-usec 1000d0))))

(defun sql-log (sql params row-count took-usec)
  (dolist (hook-fn *sql-execution-hooks*)
    (funcall hook-fn sql params row-count took-usec))
  (values))

#+sbcl
(defmacro with-took-usec (var &body body)
  (let ((before-unixtime (gensym "BEFORE-UNIXTIME"))
        (before-usec (gensym "BEFORE-USEC"))
        (after-unixtime (gensym "AFTER-UNIXTIME"))
        (after-usec (gensym "AFTER-USEC")))
    `(multiple-value-bind (,before-unixtime ,before-usec)
         (sb-ext:get-time-of-day)
       (multiple-value-prog1 (progn ,@body)
         (multiple-value-bind (,after-unixtime ,after-usec)
             (sb-ext:get-time-of-day)
           (setf ,var (+ (* (- ,after-unixtime ,before-unixtime)
                            1000000)
                         (- ,after-usec ,before-usec))))))))

#-sbcl
(defmacro with-took-usec (var &body body)
  (let ((before (gensym "BEFORE")))
    `(let ((,before (get-internal-real-time)))
       (multiple-value-prog1 (progn ,@body)
         (setf ,var (ceiling
                      (* (- (get-internal-real-time) ,before)
                         #.(/ 1000000 internal-time-units-per-second))))))))
