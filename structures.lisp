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
;;; or auto-terminate at max length), more used in fatherland (any negative is
;;; terminator, error rather than auto-term at max length, no max length, and
;;; calendars), and also the sparse array (single terminator, multiple rules for
;;; skipping or discarding values based on keys). Try to reduce that down to
;;; fewer, more parametrizable maps.
;;; GENERAL TODO: A bunch of things defined as "types" that it might be nice to
;;; have as classes? Or that might not be necessary.
;;; TODO: raw-bytes is redundant with fixed-length-list, and fixed-length-list
;;; could reasonably return an array instead of a list. Pare down the
;;; dependencies.


;;; Map reduction planning notes:
;;; Variations:
;;; 1. sparse-array has: specific-value terminator, range to not read a value
;;; and go to next key, other range to read a value and ignore it.
;;; 2. length-capped-terminated-map has: specific-value terminator, max-length terminator
;;; 3. negative-terminated-map has: range terminator
;;; 4. calendars has: range terminator to end immediately, other range terminator
;;; to end after reading next value, concluding signature.
;;; Commonalities of readers:
;;; - All readers are a loop
;;; - Three read loops open with "for key = (read-value key-type in)"
;;;   - length-capped-terminated-map needs a repeat before that, though
;;; - All read loops then have an until with some predicate
;;; - Three read loops then have a "for value = (read-value value-type in)" and
;;; then collect that in a dotted pair with the key
;;;   - dom-sparse-array instead only reads sometimes, so its read expression is
;;;   more complex
;;; - calendars has a second until after the collector (need to check syntax
;;; there) and a finally. Others have nothing after collector.
;;; Commonalities of writers:
;;; - loop for key . value in values
;;;   - calendars doesn't bother to extract because key and value are the same
;;;   type, but that's fixable
;;; - length-capped-terminated-map discards excess length here
;;; - in three cases, write key and value.
;;;   - dom-sparse-array handles possible value skips here.
;;; - all of them have a finally to write some kind of terminator, of varying complexity
;;; TODO write macros for map-reader and map-writer


(define-binary-type dom-sparse-array (filter)
  ;;; Sparse array: paired (i32, byte) until key is negative, sometimes skipping data.
  ;; Go code would:
  ;; - discard any negative key without reading a value
  ;; - discard any key >= filter, but read-and-discard the value byte
  ;; - only end on a key of -1
  ;; My slightly different implementation allows looking at the discarded stuff
  ;; without altering the writeback. As such, it currently ignores the filter.
  ;; might be better to note the skip in metadata somehow?
  (:reader (in)
           (loop for key = (read-value 'p:i32 in)
                 until (eql key -1)
                 for value = (if (>= key 0) (read-value 'raw-bytes in :length 1) nil)
                 collecting (cons key value)))
  (:writer (out pairs)
           (loop for (key . value) in pairs
                 do (write-value 'p:i32 out key)
                 when (< key 0) do (write-value 'raw-bytes out value :length 1)
                 finally (write-value 'p:i32 out -1))))

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
  (:writer (out values)
           (loop for (key . value) in values
                 do (progn (write-value key-type out key)
                           (write-value value-type out value))
                 ;; TODO if we save the specific terminator, use that instead of -1
                 finally (write-value key-type out -1))))

(define-binary-type calendars ()
  ;; One key to two values, terminate before reading values for negative key,
  ;; terminate after reading values for key over 999, expect a signature #x205B
  ;; after termination. Used in fatherland.
  (:reader (in)
           ;; TODO test that this double-until works properly
           ;; TODO this should be able to fold up into the other maps with some work
           (loop for key = (read-value 'b:i32 in)
                 until (< key 0)
                 for value = (read-value 'fixed-length-list in :length 2 :value-type 'b:i32)
                 collecting (cons key value)
                 until (> key 999)
                 finally (read-value 'b:sentinel :type 'b:i32 :expected #x205B)))
  (:writer (out values)
           ;;; TODO there are multiple possible terminations and we have no way
           ;;; to distinguish explicit-terminator (negative key) from implicit
           ;;; (high key). This code produces INCORRECT OUTPUT for the latter.
           ;;; Fix.
           (loop for triple in values
                 do (write-value 'fixed-length-list out triple :length 3 :value-type 'b:i32)
                 finally (write-value 'fixed-length-list out '(-1 #x205B) :length 2 :value-type 'b:i32))))


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
   (unk-byte-array (raw-bytes :length 51)) ; Go code notes "46 + 5" without clarification?
   (unk-16-25 p:u16)))

;;; Delayed events. Three lists of i32, interleaved, with a single prefixing length.
;;; That is, read one i32, then read that many i32 into each of "base", "turn", and "lunar".
;;; TODO implement as a class? can't be done with current macros but can still
;;; use the same read/write interface, maybe.
(define-binary-type delayed-events ()
  (:reader (in)
           (let ((size (read-value 'p:i32 in)))
             (loop repeat size
                   ;; TODO double-check loop syntax, maybe roll outer let into loop?
                   collecting (read-value 'p:i32 in) into base
                   collecting (read-value 'p:i32 in) into turn
                   collecting (read-value 'p:i32 in) into lunar
                   return `((:size size) (:base base) (:turn turn) (:lunar lunar)))))
  (:writer (out value)
           (destructuring-bind ((:size size) (:base base) (:turn turn) (:lunar lunar)) value
             (write-value 'p:i32 out size)
             (loop for b in base
                   for t in turn
                   for l in lunar
                   do (progn
                        (write-value 'p:i32 out b)
                        (write-value 'p:i32 out t)
                        (write-value 'p:i32 out l))))))
;; alternate definition for delayed-events, less code to maintain, but structure
;; is maybe not as nice to work with, depending on what the three fields
;; actually *mean*.
;; TODO pick one of these two.
;; (define-binary-type event-triple () (fixed-length-list :length 3 :value-type 'p:i32))
;; (define-binary-type delayed-events () (variable-length-list :length-type 'p:i32 :value-type 'event-triple))

(define-binary-class dominion ()
  ((sentinel (p:sentinel :type 'p:u16 :expected 12346))
   (b08l06 (raw-bytes :length 6))
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

;;; End-stats. Eight lists of i16, with a single prefixing length.
;;; Not interleaved. Actual length is 200 times prefix.
;;; Maybe corresponds to score graphs?
(define-binary-type end-stats ()
  (:reader (in)
           (let* ((size (read-value 'p:i16 in))
                  (real-size (* 200 size)))
             (list size
                   (loop repeat 8
                         with real-size = (* 200 size)
                         collecting (read-value 'fixed-length-list in :length real-size :value-type 'p:i16)))))
  (:writer (out value)
           (destructuring-bind (size stats) value
             (write-value 'p:i16 out size)
             (loop for stat-list in stats
                   with real-size = (* 200 size)
                   do (write-value 'fixed-length-list out stat-list :length real-size :value-type 'p:i16)))))

(define-binary-type rxor-50 () (p:string-rxor :length 50)) ; used in fatherland, can't pass keys for nested types

(define-binary-class fatherland ()
  ;; The top-level structure of the file
  ((header header)
   (settings settings)
   (calendars calendars)
   (zoom ...) ; TODO uncertain of type; Go uses "binary.LittleEndian"
   ;; NOTE: in the Go, the lands read here are the "treatAsFatherland" variant.
   (lands (negative-terminated-map :key-type 'p:i32 :value-type 'land)) ; TODO error over #x5E0
   (kingdoms (negative-terminated-map :key-type 'p:i32 :value-type 'kingdom)) ; TODO error over #xF9
   (units (negative-terminated-map :key-type 'p:i32 :value-type 'unit))
   (commanders (negative-terminated-map :key-type 'p:i32 :value-type 'commander))
   (dominions (negative-terminated-map :key-type 'p:i32 :value-type 'dominion))
   (spells (negative-terminated-map :key-type 'p:i32 :value-type 'spell-data))
   (mercenaries (negative-terminated-map :key-type 'p:i32 :value-type 'mercenary-data)) ; NOTE: Go code comments "something weird"
   (merc-unknown (raw-bytes :length 100))
   (enchantments (negative-terminated-map :key-type 'p:i32 :value-type 'enchantment-data))
   (items (raw-bytes :length 1000))
   (war-data (raw-bytes :length 40000)) ; what?? all the battles, maybe? but that doesn't make sense as fixed-size.
   (heroes (variable-length-list :length-type 'p:i32 :value-type 'p:i16))
   (unk-rolling (fixed-length-list :length 200 :value-type 'rxor-50))
   (end-stats end-stats)
   (event-occurrences ...) ; TODO first a value which must be >= 4474. Then either 1000 events, or (if the value was exactly 4475) an i32 saying how many events there are. Each event is an i16.
   (delayed-events-sentinel (p:sentinel :type 'p:i32 :expected 4480))
   (delayed-events delayed-events)
   (closing-sentinel (p:sentinel :type 'p:i32 :expected 12346)))) ; TODO assert the file is over

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
   (sentinel (p:sentinel :type 'p:i32 :expected 12346))))

(define-binary-class kingdom ()
  ;; NOTE: Go code doesn't even save most of the unknowns, just discards.
  ((sentinel-1 (p:sentinel :type 'p:i16 :expected 12346))
   (unk-i32-00 p:i32)
   (unk-28x16 (fixed-length-list :length 28 :type 'p:u16))
   (unk-varlen-u16 (variable-length-list :length-type 'p:i32 :value-type 'p:u16))
   (unk-29x16 (fixed-length-list :length 29 :type 'p:u16))
   (leader-name p:string)
   (unk-9-bytes (raw-bytes :length 9))
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
   (unk-byte-00 (raw-bytes :length 1))
   (unk32-01 p:u32)
   (unk-byte-01 (raw-bytes :length 1))))
