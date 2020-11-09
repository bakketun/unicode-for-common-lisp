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
               (:file "scalar-value-sequence")
               (:file "code-unit-string")
               (:file "char-code-unit-string")
               (:file "byte-vector-code-unit-string")
               (:file "utf-8-string")
               (:file "utf-16-string")
               (:file "utf-32-string")))
