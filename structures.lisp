(defpackage #:dominions-parser/structures
  (:use #:cl)
  (:import-from #:com.gigamonkeys.binary-data
                #:define-binary-type #:define-binary-class #:define-tagged-binary-class #:read-value #:write-value)
  (:local-nicknames (#:p #:dominions-parser/prims/all))
  (:export)) ; TODO
(in-package #:dominions-parser/structures)

;;; GENERAL TODO: A bunch of things defined as "types" that it might be nice to
;;; have as classes? Or that might not be necessary.

;;; TODO maybe separate out the more generic lists and maps from the rest

(defmacro map-reader (&key (key 'key-type) (val 'value-type) pre-read key-check post-read (stream 'in))
  "Common code to four different map readers"
  (declare (type (or symbol list) key val stream)
           (type list pre-read key-check post-read))
  (flet ((type-to-reader (s)
           (if (or (symbolp s) (eql (first s) 'quote))
               `(read-value ,s ,stream)
               s)))
    (let ((key-reader (type-to-reader key))
          (val-reader (type-to-reader val)))
      `(loop ,@pre-read
             for key = ,key-reader
             until ,key-check
             for value = ,val-reader
             collecting (cons key value)
             ,@post-read))))

(defmacro map-writer (&key (key 'key-type) (val 'value-type) pre-write epilogue (stream 'out) (input 'items))
  "Common code to four different map writers"
  (declare (type (or symbol list) key val stream input)
           (type list pre-write epilogue))
  (flet ((type-to-writer (s i)
           (if (or (symbolp s) (eql (first s) 'quote))
               `(write-value ,s ,stream ,i)
               s)))
    (let ((key-writer (type-to-writer key 'key))
          (val-writer (type-to-writer val 'value)))
      `(loop for (key . value) in ,input
             ,@pre-write
             do (progn ,key-writer ,val-writer)
             finally ,@epilogue))))

(define-binary-type dom-sparse-array (filter)
  ;;; Sparse array: paired (i32, byte) until key is negative, sometimes skipping data.
  ;; Go code would:
  ;; - discard any negative key without reading a value
  ;; - discard any key >= filter, but read-and-discard the value byte
  ;; - only end on a key of -1
  ;; My slightly different implementation allows looking at the discarded stuff
  ;; without altering the writeback. As such, it currently ignores the filter.
  ;; Might be better to note the skip in metadata somehow?
  (:reader (in)
           (map-reader
            :key 'p:i32 :key-check (eql key 1)
            :val (if (>= key 0) (read-value 'p:u8 in) nil)))
  (:writer (out items)
           (map-writer
            :key 'p:i32
            :val (unless (< key 0)
                         (write-value 'p:u8 out value))
            :epilogue ((write-value 'p:i32 out -1)))))

(define-binary-type length-capped-terminated-map (key-type value-type terminator max-length)
  ;; Key-value pairs, terminated by a specific key of known type OR by reaching a set max length.
  ;; If max length is reached, no terminator is present.
  ;; Used specifically in dominion structure
  (:reader (in)
           (map-reader :pre-read (repeat max-length) :key-check (eql key terminator)))
  (:writer (out items)
           ;; TODO currently silently discards input in excess of max length
           (map-writer
            :pre-write (for count from 1 upto max-length)
            :epilogue ((if (> max-length count)
                          (write-value key-type out terminator))))))

(define-binary-type negative-terminated-map (key-type value-type)
  ;; Key-value pairs, terminated by any negative-number key (key-type must be numeric)
  ;; TODO Go code has *errors* if the length exceeds a limit
  ;; (rather than the auto-termination in length-capped-terminated-map)
  ;; TODO should the terminator be preserved, or just write a -1? Currently the latter.
  (:reader (in)
           (map-reader :key key-type :key-check (< key 0)))
  (:writer (out items)
           (map-writer :epilogue ((write-value key-type out -1)))))

(define-binary-type i32-pair () (fixed-length-list :length 2 :value-type 'p:i32))

(define-binary-type calendars ()
  ;; One key to two values, terminate before reading values for negative key,
  ;; terminate after reading values for key over 999, expect a signature #x205B
  ;; after termination. Used in fatherland.
  (:reader (in)
           ;; TODO test that this double-until works properly
           (map-reader
            :key 'p:i32
            :key-check (< key 0)
            :val 'i32-pair
            :post-read (until (> key 999)
                        finally (read-value 'p:sentinel :type 'p:i32 :expected #x205B))))
  (:writer (out items)
           ;;; TODO there are multiple possible terminations and we have no way
           ;;; to distinguish explicit-terminator (negative key) from implicit
           ;;; (high key). This code produces INCORRECT OUTPUT for the latter.
           ;;; FIXME.
           (map-writer
            :key 'p:i32 :val 'i32-pair
            :epilogue ((write-value 'i32-pair out '(-1 #x205B))))))

;;; NOTE: read-sequence can (probably (significantly)) improve efficiency of the
;;; two list readers, for unsigned integer lists where lisp can auto-handle the
;;; reads, but it would complicate the code to special-case such things.
;;; Consider if necessary. Maybe separate off an "array" type to be used for
;;; such numeric cases.
(define-binary-type fixed-length-list (length value-type)
  ;; Array of fixed size, determined by outside context.
  (:reader (in)
           (loop repeat length
                 collecting (read-value value-type in)))
  (:writer (out values)
           ;; TODO currently does not check against length
           (loop for item in values
                 do (write-value value-type out item))))

(define-binary-type variable-length-list (length-type value-type)
  (:reader (in)
           (let ((length (read-value length-type in)))
             (read-value 'fixed-length-list :length length :value-type value-type)))
  (:writer (out values)
           (let ((length (length values)))
             (write-value length-type out length)
             (write-value 'fixed-length-list out values :length length :value-type value-type))))

;;; General NOTE: Lots of these structures have unknown fields. I am reading
;;; them as lists where the types are consistent for long blocks, for
;;; conciseness. I make notes in each class as to whether its unknown-meaning
;;; lists were read individually or as arrays in the Go code.

(define-binary-class commander ()
  ;; lot of unknown fields. Go code reads individually.
  ((name p:string-term)
   (unk-6x32 (fixed-length-list :length 6 :value-type 'p:u32))
   (unk-16x16 (fixed-length-list :length 16 :value-type 'p:u16))
   (unk-5x32 (fixed-length-list :length 5 :value-type 'p:u32))
   (unk-9x16 (fixed-length-list :length 9 :value-type 'p:u16))
   (unk-byte-array (fixed-length-list :length 51 :value-type 'p:u8)) ; Go code notes "46 + 5" without clarification?
   (unk-16-25 p:u16)))

(define-binary-type delayed-events ()
  ;;; Delayed events. Three lists of i32, interleaved, with a single prefixing
  ;;; length. That is, read one i32, then read that many i32 into each of
  ;;; "base", "turn", and "lunar".
  ;;; TODO implement as a class? Can't be done with current macros but can still
  ;;; use the same read/write interface, maybe.
  (:reader (in)
           (loop with size = (read-value 'p:i32 in)
                 repeat size
                 for (a b c) = (read-value 'fixed-length-list :length 3 :value-type 'p:i32)
                 collecting a into base
                 collecting b into turn
                 collecting c into lunar
                 finally (return `(:size ,size :base ,base :turn ,turn :lunar ,lunar))))
  (:writer (out value)
           (destructuring-bind (&key size base turn lunar) value
             (write-value 'p:i32 out size)
             (loop for a in base
                   for b in turn
                   for c in lunar
                   do (progn
                        (write-value 'p:i32 out a)
                        (write-value 'p:i32 out b)
                        (write-value 'p:i32 out c))))))
;; alternate definition for delayed-events, less code to maintain, but structure
;; is maybe not as nice to work with, depending on what the three fields
;; actually *mean*.
;; TODO pick one of above or below.
;; (define-binary-type i32-triple () (fixed-length-list :length 3 :value-type 'p:i32))
;; (define-binary-type delayed-events () (variable-length-list :length-type 'p:i32 :value-type 'i32-triple))

(define-binary-class dominion ()
  ((sentinel (p:sentinel :type 'p:u16 :expected 12346))
   (b08l06 (fixed-length-list :length 6 :value-type 'p:u8))
   (name p:string-term)
   (unk-u32-00 p:u32)
   (unk-u32-01 p:u32)
   (unk-map (length-capped-terminated-map :key-type 'p:i32 :value-type 'p:i32 :terminator 0 :max-length 64))))

(define-binary-class enchantment-data ()
  ;; Go code has these as ints. Reads individually.
  ((sentinel (p:sentinel :type 'p:i16 :expected 26812))
   (unk-i32-00 p:i32)
   (unk-6x16 (fixed-length-list :length 6 :value-type 'p:i16))
   (unk-5x32 (fixed-length-list :length 5 :value-type 'p:i32))
   (unk-i16-06 p:i16)))

(define-binary-type end-stats ()
  ;;; End-stats. Eight lists of i16, with a single prefixing length. Not
  ;;; interleaved. Actual length is 200 times prefix.
  ;;; Maybe corresponds to score graphs?
  (:reader (in)
           (let* ((size (read-value 'p:i16 in))
                  (real-size (* 200 size)))
             (cons size
                   (loop repeat 8
                         collecting (read-value 'fixed-length-list in
                                                :length real-size :value-type 'p:i16)))))
  (:writer (out value)
           (destructuring-bind (size &rest stats) value
             (write-value 'p:i16 out size)
             (loop for stat-list in stats
                   with real-size = (* 200 size)
                   do (write-value 'fixed-length-list out stat-list
                                   :length real-size :value-type 'p:i16)))))

(define-binary-type rxor-50 () (p:string-rxor :length 50)) ; used in fatherland, can't pass keys for nested types

(define-tagged-binary-class event-occurrences ()
  ;; TODO first a value which must be >= 4474. Then either 1000 events, or (if the value was exactly 4475) an i32 saying how many events there are. Each event is an i16.
  ...) ; TODO

(define-binary-class fatherland ()
  ;; The top-level structure of the file
  ((header header)
   (settings settings)
   (calendars calendars)
   (zoom p:f32) ; NOTE: little-endian, but this is a low-priority part of the parse.
   ;; NOTE: in the Go, the lands read here are the "treatAsFatherland" variant.
   (lands (negative-terminated-map :key-type 'p:i32 :value-type 'land)) ; TODO error over #x5E0
   (kingdoms (negative-terminated-map :key-type 'p:i32 :value-type 'kingdom)) ; TODO error over #xF9
   (units (negative-terminated-map :key-type 'p:i32 :value-type 'unit))
   (commanders (negative-terminated-map :key-type 'p:i32 :value-type 'commander))
   (dominions (negative-terminated-map :key-type 'p:i32 :value-type 'dominion))
   (spells (negative-terminated-map :key-type 'p:i32 :value-type 'spell-data))
   (mercenaries (negative-terminated-map :key-type 'p:i32 :value-type 'mercenary-data)) ; NOTE: Go code comments "something weird"
   (merc-unknown (fixed-length-list :length 100 :value-type 'p:u8))
   (enchantments (negative-terminated-map :key-type 'p:i32 :value-type 'enchantment-data))
   (items (fixed-length-list :length 1000 :value-type 'p:u8))
   (war-data (fixed-length-list :length 40000 :value-type 'p:u8)) ; what?? all the battles, maybe? but that doesn't make sense as fixed-size.
   (heroes (variable-length-list :length-type 'p:i32 :value-type 'p:i16))
   (unk-rolling (fixed-length-list :length 200 :value-type 'rxor-50))
   (end-stats end-stats)
   (event-occurrences event-occurrences)
   (delayed-events-sentinel (p:sentinel :type 'p:i32 :expected 4480))
   (delayed-events delayed-events)
   (closing-sentinel (p:sentinel :type 'p:i32 :expected 12346)))) ; TODO assert the file is over

(define-binary-class header ()
  ((signature (fixed-length-list :length 6 :value-type 'p:u8)) ; TODO assert is #x01 #x02 #x04 #x44 #x4F #x4D
   (user-id p:i32)
   (game-version p:i32) ; TODO assert is not too old? maybe? or at least warn.
   (turn-number p:i32)
   (unk-i32-00 p:i32)
   (unk-i32-01 p:i32)
   (realm-id p:i32)
   (unk-i32-02 p:i32)
   (unk-i32-03 p:i32)
   (game-name p:string-term)
   (password p:string-rxor-term)
   (master-password p:string-rxor-term)
   (turn-key p:i32)
   (sentinel (p:sentinel :type 'p:i32 :expected 12346))))

(define-binary-class kingdom ()
  ;; NOTE: Go code doesn't even save most of the unknowns, just discards.
  ((sentinel-1 (p:sentinel :type 'p:i16 :expected 12346))
   (unk-i32-00 p:i32)
   (unk-28x16 (fixed-length-list :length 28 :type 'p:u16))
   (unk-varlen-u16 (variable-length-list :length-type 'p:i32 :value-type 'p:u16))
   (unk-29x16 (fixed-length-list :length 29 :type 'p:u16))
   (leader-name p:string)
   (unk-9-bytes (fixed-length-list :length 9 :value-type 'p:u8))
   ;; these divisions into blocks are from the Go
   (unk-81x16 (fixed-length-list :length 81 :type 'p:u16))
   (unk-u16 p:u16)
   (unk-200x16-a (fixed-length-list :length 200 :type 'p:u16))
   (unk-200x16-b (fixed-length-list :length 200 :type 'p:u16))
   (unk-i32-01 p:i32)))

;;; TODO: Land. Go code has two different reads based on a "treatAsFatherland"
;;; bool that doesn't seem to be part of the file, just the context. I think the
;;; false case is (part of?) the 2h file, rather than the trn file.

(define-binary-class mercenary-data ()
  ((name p:string)
   (unk-3x16 (fixed-length-list :length 3 :value-type 'p:u16))
   (unk-15x16 (fixed-length-list :length 15 :value-type 'p:u16))
   (unk-5x16 (fixed-length-list :length 5 :value-type 'p:u16)) ; NOTE: Go code has these five as individual
   (sentinel (p:sentinel :type 'p:i16 :expected 26812))))

(define-binary-class newlord ()
  ((header header)
   (unk-u32-00 p:u32)
   (unk-u32-01 p:u32)
   (unit unit)
   (commander commander)
   (dominion dominion)
   (unk-u32-02 p:u32)))

(define-binary-class game-mod ()
  ((major-version p:i16)
   (minor-version p:i16)
   (name p:string)
   (unk-u32-00 p:u32)
   (unk-u32-01 p:u32)))

(define-binary-class settings ()
  ((mode p:u8)
   (unk-u8-00 p:u8)
   (autoplay p:u8)
   (strength-of-indies p:u8)
   (site-frequency p:u8)
   (event-frequency p:u8)
   (hall-of-fame-size p:u8)
   (research-difficulty p:u8)
   (map-wrap p:u8)
   (winner-id p:u8)
   (unk-u8-01 p:u8)
   ;; TODO victory mode has a counter and then a bunch of different possible
   ;; reads depending on its value. Also, game-era is inserted in the middle of
   ;; the counter-dependent stuff. Tagged class?
   (victory-mode-settings ...)
   (unk-i32-00 p:i32)
   (sail-distance p:i32)
   (mods (variable-length-list :length-type 'p:i16 :value-type 'game-mod))
   (map-picture p:string)
   ;; NOTE: Go code reads colors as 4xi32, then converts to u32, then to float32.
   (colors (fixed-length-list :length 4 :value-type 'p:i32))))

(define-binary-class spell-data ()
  ;; Go code reads as arrays.
  ((unk-16-x1000 (fixed-length-list :length 1000 :value-type 'p:u16))
   (unk-16-x24 (fixed-length-list :length 24 :value-type 'p:u16))))

(define-binary-class unit ()
  ;; lot of unknown fields, Go code notes array structure but reads individually?
  ((unk-3x64 (fixed-length-list :length 3 :value-type 'p:u64))
   (unk-32-a-00 p:u32)
   (unk-14x16 (fixed-length-list :length 14 :value-type 'p:u16))
   ;; Go notes this u16 as separate from the previous array of 14
   (unk-16-b p:u16)
   (unk-byte-00 (fixed-length-list :length 1 :value-type 'p:u8))
   (unk32-01 p:u32)
   (unk-byte-01 p:u8)))
