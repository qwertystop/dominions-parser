(uiop/package:define-package #:binparse/generics
  (:use #:common-lisp)
  (:import-from #:com.gigamonkeys.binary-data #:read-value #:write-value)
  (:export #:read-file #:write-file))
(in-package #:binparse/generics)

;;; TODO these could be more concise as macros or with some other way of splicing in keys

(defun read-file (type filespec &rest keys)
  (with-open-file (filestream filespec :element-type '(unsigned-byte 8))
    (if keys
        (apply #'read-value `(,type ,filestream ,@keys))
        (read-value type filestream))))

(defun write-file (type filespec value &rest keys)
  (with-open-file (filestream filespec :direction :output :element-type '(unsigned-byte 8))
    (if keys
        (apply #'write-value `(,type ,filestream ,value ,@keys))
        (write-value type filestream value))))
