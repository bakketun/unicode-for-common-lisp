(asdf:defsystem #:unicode-base
  :name "unicode-base"
  :licence "MIT"
  :author "Thomas Bakketun <thomas@bakketun.pro>"
  :description "Unicode support for Common Lisp."
  :depends-on ()
  :serial t
  :components ((:file "package")
               (:file "constants")
               (:file "features")
               (:file "types")
               (:file "code-unit-sequence")
               (:file "code-unit-string")))
