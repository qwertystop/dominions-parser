(defpackage "binary-parser/src/formats/dominions/numbers"
  (:use "cl")
  (:import-from "com.gigamonkeys.binary-data"
                #:define-binary-type #:read-value #:write-value)
  (:export #:f32 #:f32-special
           #:var-len-int))
(in-package "binary-parser/src/formats/dominions/numbers")

;;; Variable-length integer, little-endian, Dominions-specific
(define-binary-type var-len-int ()
  ;; Read a byte. If the high bit is 1, drop it and read two bytes. If the high
  ;; bit of that is 1, drop it and read four bytes. No, don't know why they did
  ;; it this way. Note that the Go code is unclear here – it reads signed ints
  ;; but then checks if they're greater than the largest positive value for
  ;; signed ints of that size? Currently assuming untested and it was meant to
  ;; be unsigned. TODO test. Note also that the Go code dates to Dom4, or in
  ;; some places Dom3, and it's possible that this has been expanded to 64-bit
  ;; ints, or otherwise rewritten.
  ;; TODO this can probably be cleaned up?
  (:reader (in)
           (let ((value (read-value in 'u8)))
             (if (logbitp 8 value)
                 (progn
                   (setf value (read-value in 'u16))
                   (if (logbitp 16 value)
                       (progn
                         (setf value (read-value in 'u32))))))
             value))
  (:writer (out value)
           (declare ((integer 0 #xFFFF) value))
           (if (<= value #x7F)
               (write-value 'u8 out value)
               (progn
                 (write-value 'u8 out #x80)
                 (if (<= value #x7FFF)
                     (write-value 'u16 out value)
                     (progn
                       (write-value 'u16 out #x8000)
                       (write-value 'u32 out value)))))))

;;; Floats
;;; Dominions stores floats in two different ways and I don't understand why?
;;; NOTE: Going by the Go code, the regular floats are only used for extremely
;;; low-value cosmetic purposes, and the special ones not at all. Can probably
;;; afford to table this for now.
;;; So here's placeholders to read the proper amount of data. (TODO)
(define-binary-type f32 () u32)
(define-binary-type f32-special () u32)
;;; (define-binary-type f32 ()
;;;   (:reader (in)
;;;            ...)
;;;   (:writer (out value)
;;;            ...))

;;; (define-binary-type f32-special ()
;;;   (:reader (in)
;;;            ...)
;;;   (:writer (out value)
;;;            ...))
