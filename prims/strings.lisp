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

;;; According to the Go code, Dominions has both terminated and fixed-length
;;; strings, and uses either a fixed or rolling mask to XOR each character
;;; before saving it, apparently purely for obfuscation. I have confirmed that
;;; this mask is still accurate on Dom5 save files.

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

;;; -----
;;; Rolling-mask strings
;;; -----

(defun roll-mask (code mask)
  "Single-byte adder, discarded carry"
  (ldb (byte 8 0) (+ code mask)))

;;; Rolling-masks have an initial mask of #x78; I would put it as a default
;;; argument but PCL's define-binary-type doesn't work with generic arguments.
(defconstant +ROLLING-MASK-INITIAL+ #x78)

;;; TODO re-implement with a masked-byte-char? Would still need to have a custom
;;; string wrapper though, to roll the mask. Might not help much.
(define-binary-type dom-string-rxor (length)
  (:reader (in)
           (with-output-to-string (s)
             (loop repeat length
                   for mask = +ROLLING-MASK-INITIAL+ then (roll-mask code mask)
                   for code = (logxor mask (read-byte in))
                   until (= 0 code)
                   do (write-char (code-char code) s))))
  (:writer (out string)
           (loop repeat length
                 for char across string
                 for mask = +ROLLING-MASK-INITIAL+ then (roll-mask code mask)
                 for code = (logxor mask (char-code char))
                 do (write-byte code out))))

(define-binary-type dom-string-rxor-term ()
  (:reader (in)
           (with-output-to-string (s)
             (loop for mask = +ROLLING-MASK-INITIAL+ then (roll-mask code mask)
                   for code = (logxor mask (read-byte in))
                   until (= 0 code)
                   do (write-char (code-char code) s))))
  (:writer (out string)
           (loop for char across string
                 for mask = +ROLLING-MASK-INITIAL+ then (roll-mask code mask)
                 for code = (logxor mask (char-code char))
                 do (write-byte code out)
                 finally (write-byte mask out))))
