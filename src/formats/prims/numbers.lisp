(defpackage "binary-parser/src/formats/prims/numbers"
  (:use "cl")
  (:import-from "com.gigamonkeys.binary-data"
                #:define-binary-type #:read-value #:write-value)
  (:export #:u8 #:u16 #:u32 #:u64
           #:i8 #:i16 #:i32 #:i64
           #:sentinel))
(in-package "binary-parser/src/formats/prims/numbers")

;;; -----
;;; Normal integers, arbitrary length (8-bit bytes), either endianness
;;; -----

(define-binary-type unsigned-int (length is-big)
  (:reader (in)
            (loop with result = 0
                  with cap = (max-offset length)
                  for i from 0 upto cap by 8
                  for v = (if is-big (- cap i) i)
                  do (setf (ldb (byte 8 v) result) (read-byte in))
                  finally (return result)))
  (:writer (out value)
           (declare ((integer 0) value))
           (loop with cap = (max-offset length)
                 for i from 0 upto cap by 8
                 for v = (if is-big i (- cap i)) ; inverse of reader
                 do (write-byte (ldb (byte 8 v) value) out))))

(define-binary-type signed-int (length is-big)
  ;; NOTE: CL's bitwise ops always return positive integers, and when given negatives,
  ;; treat them as twos-complement of minimal bit length.
  (:reader (in)
           (declare ((integer 0) length))
           (let ((unsigned (read-value 'unsigned-int in :length length :is-big is-big)))
             (unsigned-to-twos-comp unsigned (* 8 length))))
  (:writer (out value)
           (declare (integer value))
           (declare ((integer 0) length))
           (write-value 'unsigned-int out (twos-comp-to-unsigned value (* 8 length))
                        :length length :is-big is-big)))

;;; Unsigned integers, all little-endian
(define-binary-type u8 () (unsigned-int :length 1 :is-big nil))
(define-binary-type u16 () (unsigned-int :length 2 :is-big nil))
(define-binary-type u32 () (unsigned-int :length 3 :is-big nil))
(define-binary-type u64 () (unsigned-int :length 4 :is-big nil))

;;; Signed integers, all little-endian
(define-binary-type i8  () (signed-int :length 1 :is-big nil))
(define-binary-type i16 () (signed-int :length 2 :is-big nil))
(define-binary-type i32 () (signed-int :length 3 :is-big nil))
(define-binary-type i64 () (signed-int :length 4 :is-big nil))


;;; -----
;;; Sentinel values
;;; -----
;;; TODO move sentinels to a misc, they're not necessarily numbers
;;; TODO set this up as proper errors with abort/ignore/replace resolutions, instead of warning prints.
(define-binary-type sentinel (type expected)
  (:reader (in)
           (let ((actual (read-value type in)))
             (unless (eql actual expected)
               (format t "WARNING: Read ~S, but expected ~S (sentinel/magic)~%" actual expected))
             actual))
  (:writer (out value)
           (unless (eql value expected)
             (format t "WARNING: Writing ~S for expected ~S (sentinel/magic)~%" value expected))
           (write-value type out value)))

;;; -----
;;; Useful math and twiddles for implementation
;;; -----
(defun max-offset (size)
  "Highest offset for reading one byte of a (size)-byte field"
  (* (- size 1) 8))

(defun unsigned-to-twos-comp (unsigned bits)
  (declare ((integer 0) unsigned bits))
  (let ((mask (expt 2 (- bits 1))))
    (- (logand (lognot mask) unsigned) (logand mask unsigned))))

(defun twos-comp-to-unsigned (signed bits)
  (declare (integer signed))
  (declare ((integer 0) bits))
  (if (< 0 signed)
      (+ (expt 2 bits) signed)
      signed))
