(defpackage #:dbi.cache.thread
  (:nicknames #:dbi.cache)
  (:use #:cl)
  (:import-from #:bordeaux-threads)
  (:export #:make-cache-pool
           #:steal-cache-table
           #:get-object
           #:cleanup-cache-pool))
(in-package #:dbi.cache.thread)

(defun make-cache-table (&optional (test-fn 'equal))
  (make-hash-table :test test-fn))

(defstruct cache-pool
  (hash (make-hash-table :test 'eq))
  (lock (bt2:make-lock :name "CACHE-POOL-LOCK"))
  cleanup-fn)

(defun steal-cache-table (pool &optional (thread (bt2:current-thread)))
  (bt2:with-lock-held ((cache-pool-lock pool))
    (or (gethash thread (cache-pool-hash pool))
        (setf (gethash thread (cache-pool-hash pool)) (make-cache-table)))))

(defun get-object (pool key)
  (let ((cache (steal-cache-table pool)))
    (gethash key cache)))

(defun (setf get-object) (object pool key)
  (let* ((cache (steal-cache-table pool))
         (old-object (gethash key cache)))
    (when (and old-object
               (cache-pool-cleanup-fn pool))
      (funcall (cache-pool-cleanup-fn pool) old-object))
    (setf (gethash key cache) object)))

(defun cleanup-cache-pool (pool &key force)
  (let ((cleanup-fn (cache-pool-cleanup-fn pool)))
    (bt2:with-lock-held ((cache-pool-lock pool))
      (maphash (lambda (thread cache)
                 (when (or force
                           (not (bt2:thread-alive-p thread)))
                   (when cleanup-fn
                     (maphash (lambda (_ conn)
                                (declare (ignore _))
                                (when conn
                                  (funcall cleanup-fn conn)))
                              cache))
                   (remhash thread (cache-pool-hash pool))))
               (cache-pool-hash pool))))
  (values))
