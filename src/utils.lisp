(defpackage #:dbi.utils
  (:use #:cl)
  (:export #:defclass/a
           #:define-condition/a
           #:random-string
           #:compile-sql
           #:escape-string))
(in-package #:dbi.utils)

(defmacro defclass/a (name superclasses slots &rest class-options)
  `(progn
     (defclass ,name ,superclasses ,slots ,@class-options)
     (deftype ,(intern (format nil "<~A>" (symbol-name name))) () ',name)
     (setf (find-class ',(intern (format nil "<~A>" (symbol-name name))))
           (find-class ',name))))

(defmacro define-condition/a (name parent-types slots &body options)
  `(progn
     (define-condition ,name ,parent-types ,slots ,@options)
     (deftype ,(intern (format nil "<~A>" (symbol-name name))) () ',name)
     (setf (find-class ',(intern (format nil "<~A>" (symbol-name name))))
           (find-class ',name))))

(defun random-string (&optional prefix)
  (format nil "~@[~A_~]~36R" prefix (random (expt 36 #-gcl 8 #+gcl 5))))

(defun escape-string (val)
  (typecase val
    (string (format nil "'~A'"
                    (ppcre:regex-replace-all "'" val "''")))
    (otherwise val)))

(defun compile-sql (sql)
  (let ((string-ranges
          (ppcre:all-matches
           (format nil "~{~A~^|~}"
                   (list "'[^\\\\']*(?:\\\\.[^\\\\']*)*'"
                         "\"[^\\\\\"]*(?:\\\\.[^\\\\\"]*)*\""
                         "`[^\\\\`]*(?:\\\\.[^\\\\`]*)*`"))
           sql)))
    (flet ((in-string-p (pos)
             (let ((n (position-if (lambda (v)
                                     (< pos v))
                                   string-ranges)))
               (and n
                    (/= 0 (mod n 2))))))
      (loop with current = 0
            for start = 0 then (1+ pos)
            for pos = (position #\? sql :start start)
            while pos
            unless (in-string-p pos)
            collect (prog1
                        (subseq sql current pos)
                      (setf current (1+ pos)))
            into parts
            finally (let ((last-part (subseq sql start pos))
                          (count (length parts)))
                      (return
                        (lambda (params)
                          (check-type params list)
                          (assert (= (length params) count))
                          (with-output-to-string (s)
                            (loop for part in parts
                                  for param in params
                                  do (write-string part s)
                                     (princ (escape-string param) s))
                            (write-string last-part s)))))))))
