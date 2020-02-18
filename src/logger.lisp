(defpackage #:dbi.logger
  (:use #:cl)
  (:export #:*sql-execution-hooks*
           #:simple-sql-logger
           #:sql-log
           #:with-took-nsec))
(in-package #:dbi.logger)

(defvar *sql-execution-hooks* '())

(defun simple-sql-logger (sql params row-count took-nsec)
  (format t "~&~<;; ~@;~A (~{~S~^, ~}) ~@[[~D row~:P]~]~@[ (~Fms)~]~:>~%"
          (list sql
                (mapcar (lambda (param)
                          (if (typep param '(simple-array (unsigned-byte 8) (*)))
                              (map 'string #'code-char param)
                              param))
                        params)
                row-count
                (/ took-nsec 1000d0))))

(defun sql-log (sql params row-count took-ms)
  (dolist (hook-fn *sql-execution-hooks*)
    (funcall hook-fn sql params row-count took-ms))
  (values))

#+sbcl
(defmacro with-took-nsec (var &body body)
  (let ((before-unixtime (gensym "BEFORE-UNIXTIME"))
        (before-nsec (gensym "BEFORE-NSEC"))
        (after-unixtime (gensym "AFTER-UNIXTIME"))
        (after-nsec (gensym "AFTER-NSEC")))
    `(multiple-value-bind (,before-unixtime ,before-nsec)
         (sb-ext:get-time-of-day)
       (multiple-value-prog1 (progn ,@body)
         (multiple-value-bind (,after-unixtime ,after-nsec)
             (sb-ext:get-time-of-day)
           (setf ,var (+ (* (- ,after-unixtime ,before-unixtime)
                            1000000)
                         (- ,after-nsec ,before-nsec))))))))

#-sbcl
(defmacro with-took-nsec (var &body body)
  (let ((before (gensym "BEFORE")))
    `(let ((,before (get-internal-real-time)))
       (multiple-value-prog1 (progn ,@body)
         (setf ,var (ceiling
                      (* (- (get-internal-real-time) ,before)
                         #.(/ 1000000 internal-time-units-per-second))))))))
