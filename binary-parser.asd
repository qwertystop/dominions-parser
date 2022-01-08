#-asdf3.1 (error "binary-parser requires ASDF 3.1 or later.")
(defsystem "binary-parser"
  :class :package-inferred-system
  :depends-on ("clog"))

;(register-system-packages
; "pcl-binary-data"
; '(#:com.gigamonkeys.binary-data))
;(register-system-packages
; "pcl-id3v2"
; '(#:com.gigamonkeys.id3v2))
