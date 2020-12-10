(defpackage #:dominions-parser/prims/strings
  (:use #:common-lisp)
  (:import-from #:com.gigamonkeys.binary-data
                #:define-binary-type)
  (:import-from #:com.gigamonkeys.id3v2
                #:generic-string #:generic-terminated-string)
  (:export
   #:dom-string
   #:dom-string-term
   #:dom-string-rxor
   #:dom-string-rxor-term))
(in-package #:dominions-parser/prims/strings)

;;; The Go code has "string" (terminated), "string N" (fixed-length),
;;; "string RX" (terminated, rolling XOR), and "string RXN" (fixed-length,
;;; rolling XOR). All have a mask thing going on. The two basic ones can
;;; probably be a special character type on the generic and generic-terminated,
;;; but the rolling-XOR both need something more custom.

(define-binary-type masked-byte-char (mask)
  (:reader (in)
           (let ((code (read-byte in)))
             (code-char (logxor code mask))))
  (:writer (out value)
           (let ((code (char-code value)))
             (if (<= 0 code #xff)
                 (write-byte (logxor code mask) out)
                 (error "Character ~c (code ~d) falls outside single-byte range" value code)))))

;;; Fixed-mask strings
(define-binary-type 4f-masked-char () (masked-byte-char :mask #x4f))

(define-binary-type dom-string (length)
  (generic-string :length length :character-type '4f-masked-char))

(define-binary-type dom-string-term ()
  (generic-terminated-string :terminator (code-char 0) :character-type 'masked-byte-char))

(defun roll-mask (code mask)
  "Single-byte adder, discarded carry"
  (ldb (byte 8 0) (+ code mask)))

;;; Rolling-mask strings
;;; TODO re-implement with a masked-byte-char? Would still need to have a custom string wrapper though. Might not help much.
(define-binary-type dom-string-rxor ((first-mask #x78) length) ; initial value of rolling mask
  (:reader (in)
           (with-output-to-string (s)
             (loop repeat length
                   for mask = first-mask then (roll-mask code mask)
                   for code = (logxor mask (read-byte in))
                   until (= 0 code)
                   do (write-char (code-char code) s))))
  (:writer (out string)
           (loop repeat length
                 for char across string
                 for mask = first-mask then (roll-mask code mask)
                 for code = (logxor mask (char-code char))
                 do (write-byte code out))))

(define-binary-type dom-string-rxor-term ((first-mask #x78)) ; initial value of rolling mask
  (:reader (in)
           (with-output-to-string (s)
             (loop for mask = first-mask then (roll-mask code mask)
                   for code = (logxor mask (read-byte in))
                   until (= 0 code)
                   do (write-char (code-char code) s))))
  (:writer (out string)
           (loop for char across string
                 for mask = first-mask then (roll-mask code mask)
                 for code = (logxor mask (char-code char))
                 do (write-byte code out)
                 finally (write-byte mask out))))
