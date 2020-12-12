(defpackage #:dominions-parser/structures
  (:use #:cl)
  (:import-from #:com.gigamonkeys.binary-data
                #:define-binary-type #:define-binary-class #:read-value #:write-value)
  (:import-from #:com.gigamonkeys.id3v2
                #:raw-bytes)
  (:local-nicknames (#:p #:dominions-parser/prims))
  (:export)) ; TODO
(in-package #:dominions-parser/structures)

;;; GENERAL TODO: Fixed-value magic numbers (read-and-assert).
;;; GENERAL TODO: There's one kind of map used in dominion, two more used in fatherland, and also the sparse array which is distinct from all three of those. Try to reduce that down to fewer, more parametrizable maps.
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
  (:writer (out values)
           (loop for (key . value) in values
                 do (write-value p:i32 out key)
                 when (< key filter) do (write-value raw-bytes out value :length 1)
                 finally (write-value p:i32 out -1))))

(define-binary-type length-capped-terminated-map (key-type value-type terminator (max-length 64))
  ;; Key-value pairs, terminated by a specific key of known type OR by reaching a set max length.
  ;; If max length is reached, no terminator is present.
  ;; Used specifically in dominion structure
  (:reader (in)
           (loop repeat max-length
                 for key = (read-value key-type in)
                 until (eql key terminator)
                 for value = (read-value value-type in)
                 collecting (cons key value)))
  (:writer (out values)
           ;; TODO currently silently discards input in excess of max length
           (loop for (key . value) in values
                 for count from 1 upto max-length
                 do (progn (write-value key-type out key)
                           (write-value value-type out value))
                 finally (if (> max-length count) ; list shorter than max
                             (write-value key-type out terminator)))))

(define-binary-type fixed-length-list (length value-type)
  ;; Array of fixed size, determined by outside context.
  (:reader (in)
           (loop repeat length
                 collecting (read-value value-type in)))
  (:writer (out values)
           ;; NOTE currently does not check against length
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

(define-binary-class commander ()
  ;; lot of unknown fields.
  ((name p:dom-string-term)
   ;; first some u32
   (unk-32-00 p:u32) (unk-32-01 p:u32) (unk-32-02 p:u32) (unk-32-03 p:u32)
   (unk-32-04 p:u32) (unk-32-05 p:u32)
   ;; then some u16
   (unk-16-00 p:u16) (unk-16-01 p:u16) (unk-16-02 p:u16) (unk-16-03 p:u16)
   (unk-16-04 p:u16) (unk-16-05 p:u16) (unk-16-06 p:u16) (unk-16-07 p:u16)
   (unk-16-08 p:u16) (unk-16-09 p:u16) (unk-16-10 p:u16) (unk-16-11 p:u16)
   (unk-16-12 p:u16) (unk-16-13 p:u16) (unk-16-14 p:u16) (unk-16-15 p:u16)
   ;; more u32
   (unk-32-06 p:u32) (unk-32-07 p:u32) (unk-32-08 p:u32) (unk-32-09 p:u32)
   (unk-32-10 p:u32)
   ;; more u16
   (unk-16-16 p:u16) (unk-16-17 p:u16) (unk-16-18 p:u16) (unk-16-19 p:u16)
   (unk-16-20 p:u16) (unk-16-21 p:u16) (unk-16-22 p:u16) (unk-16-23 p:u16)
   (unk-16-24 p:u16)
   ;; and then 51 bytes and another u16
   (unk-byte-array (raw-bytes :length 51))
   (unk-16-25 p:u16)))

;;; TODO: Delayed events. Three lists of i32, interleaved, with a single prefixing length.
;;; That is, read one i32, then read that many i32 into each of "base", "turn", and "lunar".
;;; TODO implement as a class? can't be done with current macros but can still
;;; use the same read/write interface, maybe.

(define-binary-class dominion ()
  ((sentinel (p:sentinel :type p:u16 :expected 12346))
   (b08l06 (raw-bytes :length 6))
   (name p:dom-string-term)
   (unk-u32-00 p:u32)
   (unk-u32-01 p:u32)
   (unk-map (length-capped-terminated-map :key-type p:i32 :value-type p:i32 :terminator 0))))

(define-binary-class enchantment-data ()
  ((sentinel (p:sentinel :type p:i16 :expected 26812))
   (unk-i32-00 p:i32)
   (unk-i16-00 p:i16) (unk-i16-01 p:i16) (unk-i16-02 p:i16) (unk-i16-03 p:i16)
   (unk-i16-04 p:i16) (unk-i16-05 p:i16)
   (unk-i32-01 p:i32) (unk-i32-02 p:i32) (unk-i32-03 p:i32) (unk-i32-04 p:i32)
   (unk-i32-05 p:i32)
   (unk-i16-06 p:i16)))

;;; TODO: End-stats. Eight lists of i16, with a single prefixing length.
;;; Not interleaved. Actual length is 200 times prefix.
;;; Maybe corresponds to score graphs?

(define-binary-class fatherland ()
  ;; The top-level structure of the file
  ((header header)
   (settings settings)
   (calendars calendars) ; TODO two maps interleaved, sharing keys, term negative or >999, signature #x205B after term
   (zoom ...) ; TODO uncertain of type; Go uses "binary.LittleEndian"
   (lands ...) ; TODO map i32->land, term negative, error over #x5E0
   (kingdoms ...) ; TODO map i32->kingdom, term negative, error over #xF9
   (units ...) ; TODO map i32->unit, term negative, no cap
   (commanders ...) ; TODO map i32->commander, term negative, no cap
   (dominions ...) ; TODO map i32->dominion, term negative, no cap
   (spells ...) ; TODO map i32->spell-data, term negative, no cap
   (mercenaries ...) ; TODO map i32->mercenary-data, term negative, no cap, "something weird" in comments
   (merc-unknown (raw-bytes :length 100))
   (enchantments ...) ; TODO map i32->enchantment-data, term negative, no cap
   (items (raw-bytes :length 1000))
   (war-data (raw-bytes :length 40000)) ; what??
   (heroes (variable-length-list :length-type p:i32 :value-type p:i16))
   (unk-rolling ...) ; TODO 200 rolling-mask strings, 50 bytes each
   (end-stats end-stats)
   (event-occurrences ...) ; TODO first a value which must be >= 4474. Then either 1000 events, or (if the value was exactly 4475) an i32 saying how many events there are. Each event is an i16.
   (delayed-events-sentinel (p:sentinel :type p:i32 :expected 4480))
   (delayed-events delayed-events)
   (closing-sentinel (p:sentinel :type p:i32 :expected 12346)) ; TODO assert the file is over

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
   (game-name p:dom-string-term)
   (password p:dom-string-rxor-term)
   (master-password p:dom-string-rxor-term)
   (turn-key p:i32)
   (sentinel (p:sentinel :type p:i32 :expected 12346)))) ; TODO assert is 12346

;;; TODO: Kingdom. Bunch of unknown stuff here. Fixed length, though.

;;; TODO: Land
;;; TODO: Mercenary data
;;; TODO: Settings
;;; TODO: Spell data
;;; TODO: Unit
