(defpackage #:dominions-parser/structures
  (:use #:cl)
  (:import-from #:com.gigamonkeys.binary-data
                #:define-binary-type #:define-binary-class #:read-value #:write-value)
  (:import-from #:com.gigamonkeys.id3v2
                #:raw-bytes)
  (:local-nicknames (#:p #:dominions-parser/prims))
  (:export)) ; TODO
(in-package #:dominions-parser/structures)

;;; GENERAL TODO: There's one kind of map used in dominion (single terminator,
;;; or auto-terminate at max length), two more used in fatherland (any negative
;;; is terminator, error rather than auto-term at max length, or no max length),
;;; and also the sparse array (single terminator, multiple rules for skipping or
;;; discarding values based on keys). Try to reduce that down to fewer, more
;;; parametrizable maps.
;;; GENERAL TODO: A bunch of things defined as "types" that it might be nice to have as classes? Or that might not be necessary.

(define-binary-type dom-sparse-array (filter)
  ;;; Sparse array: paired (i32, byte) until key is negative, sometimes skipping data.
  ;; Go code would:
  ;; - discard any negative key without reading a value
  ;; - discard any key >= filter, but read-and-discard the value byte
  ;; - only end on a key of -1
  ;; my slightly different implementation allows looking at the discarded stuff without altering the writeback.
  (:reader (in)
           (loop for key = (read-value p:i32)
                 until (eql key -1)
                 collecting (if (>= key 0)
                                (cons key (read-value raw-bytes in :length 1))
                                (cons key 'nil))))
  (:writer (out pairs)
           (loop for (key . value) in pairs
                 do (write-value p:i32 out key)
                 when (< key filter) do (write-value raw-bytes out value :length 1)
                 finally (write-value p:i32 out -1))))

(define-binary-type length-capped-terminated-map (key-type value-type terminator max-length)
  ;; Key-value pairs, terminated by a specific key of known type OR by reaching a set max length.
  ;; If max length is reached, no terminator is present.
  ;; Used specifically in dominion structure
  (:reader (in)
           (loop repeat max-length
                 for key = (read-value key-type in)
                 until (eql key terminator)
                 for value = (read-value value-type in)
                 collecting (cons key value)))
  (:writer (out pairs)
           ;; TODO currently silently discards input in excess of max length
           (loop for (key . value) in pairs
                 for count from 1 upto max-length
                 do (progn (write-value key-type out key)
                           (write-value value-type out value))
                 finally (if (> max-length count) ; list shorter than max
                             (write-value key-type out terminator)))))

(define-binary-type negative-terminated-map (key-type value-type)
  ;; Key-value pairs, terminated by any negative-number key (key-type must be numeric)
  ;; TODO Go code has *errors* if the length exceeds a limit
  ;; (rather than the auto-termination in length-capped-terminated-map)
  (:reader (in)
           (loop for key = (read-value key-type in)
                 until (< key 0) ; TODO should it save the specific terminator?
                 for value = (read-value value-type in)
                 collecting (cons key value)))
  (:writer (out pairs)
           (loop for (key . value) in values
                 do (progn (write-value key-type out key)
                           (write-value value-type out value))
                 finally (write-value key-type out -1)))) ; TODO if we save the specific terminator, use that.

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
             (read-value fixed-length-list :length length :value-type value-type)))
  (:writer (out values)
           (let ((length (length values)))
             (write-value length-type out length)
             (write-value fixed-length-list out values :length length :value-type value-type))))

;;; General NOTE: Lots of these structures have unknown fields. I am reading
;;; them as lists where the types are consistent for long blocks, for
;;; conciseness. I make notes in each class as to whether its unknown-meaning
;;; lists were read individually or as arrays in the Go code.

(define-binary-class commander ()
  ;; lot of unknown fields. Go code reads individually.
  ((name p:string-term)
   (unk-6x32 (fixed-length-list :length 6 :value-type p:u32))
   (unk-16x16 (fixed-length-list :length 16 :value-type p:u16))
   (unk-5x32 (fixed-length-list :length 5 :value-type p:u32))
   (unk-9x16 (fixed-length-list :length 9 :value-type p:u16))
   (unk-byte-array (raw-bytes :length 51)) ; Go code notes "46 + 5" without clarification?
   (unk-16-25 p:u16)))

;;; TODO: Delayed events. Three lists of i32, interleaved, with a single prefixing length.
;;; That is, read one i32, then read that many i32 into each of "base", "turn", and "lunar".
;;; TODO implement as a class? can't be done with current macros but can still
;;; use the same read/write interface, maybe.

(define-binary-class dominion ()
  ((sentinel (p:sentinel :type p:u16 :expected 12346))
   (b08l06 (raw-bytes :length 6))
   (name p:string-term)
   (unk-u32-00 p:u32)
   (unk-u32-01 p:u32)
   (unk-map (length-capped-terminated-map :key-type p:i32 :value-type p:i32 :terminator 0 :max-length 64))))

(define-binary-class enchantment-data ()
  ;; Go code has these as ints. Reads individually.
  ((sentinel (p:sentinel :type p:i16 :expected 26812))
   (unk-i32-00 p:i32)
   (unk-6x16 (fixed-length-list :length 6 :value-type p:i16))
   (unk-5x32 (fixed-length-list :length 5 :value-type p:i32))
   (unk-i16-06 p:i16)))

;;; TODO: End-stats. Eight lists of i16, with a single prefixing length.
;;; Not interleaved. Actual length is 200 times prefix.
;;; Maybe corresponds to score graphs?

(define-binary-type rxor-50 () (p:string-rxor :length 50)) ; used in fatherland, can't pass keys for nested types

;;; TODO "calendars" (paired, interleaved, shared-keys maps? multiple
;;; termination rules, post-termination signature. used in fatherland)

(define-binary-class fatherland ()
  ;; The top-level structure of the file
  ((header header)
   (settings settings)
   (calendars calendars) ; TODO two maps interleaved, sharing keys, term negative or >999, signature #x205B after term
   (zoom ...) ; TODO uncertain of type; Go uses "binary.LittleEndian"
   (lands (negative-terminated-map :key-type p:i32 :value-type land)) ; TODO error over #x5E0
   (kingdoms (negative-terminated-map :key-type p:i32 :value-type kingdom)) ; TODO error over #xF9
   (units (negative-terminated-map :key-type p:i32 :value-type unit))
   (commanders (negative-terminated-map :key-type p:i32 :value-type commander))
   (dominions (negative-terminated-map :key-type p:i32 :value-type dominion))
   (spells (negative-terminated-map :key-type p:i32 :value-type spell-data))
   (mercenaries (negative-terminated-map :key-type p:i32 :value-type mercenary-data)) ; NOTE: Go code comments "something weird"
   (merc-unknown (raw-bytes :length 100))
   (enchantments (negative-terminated-map :key-type p:i32 :value-type enchantment-data))
   (items (raw-bytes :length 1000))
   (war-data (raw-bytes :length 40000)) ; what??
   (heroes (variable-length-list :length-type p:i32 :value-type p:i16))
   (unk-rolling (fixed-length-list :length 200 :value-type rxor-50))
   (end-stats end-stats)
   (event-occurrences ...) ; TODO first a value which must be >= 4474. Then either 1000 events, or (if the value was exactly 4475) an i32 saying how many events there are. Each event is an i16.
   (delayed-events-sentinel (p:sentinel :type p:i32 :expected 4480))
   (delayed-events delayed-events)
   (closing-sentinel (p:sentinel :type p:i32 :expected 12346)))) ; TODO assert the file is over

(define-binary-class header ()
  ((signature (raw-bytes :length 6)) ; TODO assert is #x01 #x02 #x04 #x44 #x4F #x4D
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
   (sentinel (p:sentinel :type p:i32 :expected 12346))))

;;; TODO: Kingdom. Bunch of unknown stuff here. Go code doesn't even save most of it, just discards.

;;; TODO: Land
;;; TODO: Mercenary data
;;; TODO: Newlord
;;; TODO: Settings

(define-binary-class spell-data ()
  ;; Go code reads as arrays.
  ((unk-16-x1000 (fixed-length-list :length 1000 :value-type p:u16))
   (unk-16-x24 (fixed-length-list :length 24 :value-type p:u16))))

(define-binary-class unit ()
  ;; lot of unknown fields, Go code notes array structure but reads individually?
  ((unk-3x64 (fixed-length-list :length 3 :value-type p:u64))
   (unk-32-a-00 p:u32)
   (unk-14x16 (fixed-length-list :length 14 :value-type p:u16))
   ;; Go notes this u16 as separate from the previous array of 14
   (unk-16-b p:u16)
   (unk-byte-00 (raw-bytes :length 1))
   (unk32-01 p:u32)
   (unk-byte-01 (raw-bytes :length 1))))
