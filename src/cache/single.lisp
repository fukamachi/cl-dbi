(defpackage #:dbi.cache.single
  (:nicknames #:dbi.cache)
  (:use #:cl)
  (:export #:make-cache-pool
           #:steal-cache-table
           #:get-object
           #:cleanup-cache-pool))
(in-package #:dbi.cache.single)

(defstruct cache-pool
  (cache (make-hash-table :test 'equal))
  cleanup-fn)

(defun steal-cache-table (pool &optional thread)
  (when thread
    (warn "Specified :thread even though it's single threaded."))
  (cache-pool-cache pool))

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

;; Just do nothing since it's single-threaded and the thread is obviously alive.
(defun cleanup-cache-pool (pool &key force)
  (declare (ignore pool))
  (when force
    (let ((cache (cache-pool-cache pool))
          (cleanup-fn (cache-pool-cleanup-fn pool)))
      (when cleanup-fn
        (maphash (lambda (key conn)
                   (declare (ignore key))
                   (when conn
                     (funcall cleanup-fn conn)))
                 cache))
      (setf (cache-pool-cache pool)
            (make-hash-table :test 'equal))))
  (values))
