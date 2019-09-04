(defpackage dbi.logger
  (:use :cl)
  (:export :*sql-execution-hooks*
           :simple-sql-logger
           :sql-log
           :with-took-ms))
(in-package :dbi.logger)

(defvar *sql-execution-hooks* '())

(defun simple-sql-logger (sql params row-count took-ms)
  (format t "~&~<;; ~@;~A (~{~S~^, ~}) ~@[[~D row~:P]~]~@[ (~Dms)~]~:>~%"
          (list sql
                (mapcar (lambda (param)
                          (if (typep param '(simple-array (unsigned-byte 8) (*)))
                              (map 'string #'code-char param)
                              param))
                        params)
                row-count
                took-ms)))

(defun sql-log (sql params row-count took-ms)
  (dolist (hook-fn *sql-execution-hooks*)
    (funcall hook-fn sql params row-count took-ms))
  (values))

(defmacro with-took-ms (took-ms &body body)
  (let ((before (gensym "BEFORE")))
    `(let ((,before (get-internal-real-time)))
       (multiple-value-prog1 (progn ,@body)
         (setf ,took-ms (- (get-internal-real-time) ,before))))))
