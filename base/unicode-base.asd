(asdf:defsystem #:unicode-base
  :name "unicode-base"
  :licence "MIT"
  :author "Thomas Bakketun <thomas@bakketun.pro>"
  :description "Unicode support for Common Lisp."
  :depends-on (#:unicode-common)
  :serial t
  :components ((:file "package")
               (:file "code-unit-string")
               (:file "char-code-unit-string")
               (:file "byte-vector-code-unit-string")
               (:file "utf-8-encode")
               (:file "utf-8-string")
               (:file "utf-16-string")
               (:file "utf-32-string")
               ))
