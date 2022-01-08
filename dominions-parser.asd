#-asdf3.1 (error "dominions-parser requires ASDF 3.1 or later.")
(defsystem "dominions-parser"
  :class :package-inferred-system
  :depends-on ("dominions-parser/src/prims/all"
               "dominions-parser/src/structures"
               "dominions-parser/src/file"))

(register-system-packages
 "pcl-binary-data"
 '(#:com.gigamonkeys.binary-data))
(register-system-packages
 "pcl-id3v2"
 '(#:com.gigamonkeys.id3v2))
