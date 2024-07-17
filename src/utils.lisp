(defpackage #:dbi.utils
  (:use #:cl)
  (:export #:defclass/a
           #:define-condition/a
           #:random-string))
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
