(defsystem #:unicode
  :name "unicode"
  :licence "TODO"
  :author "Thomas Bakketun <thomas.bakketun@copyleft.no>"
  :description "Unicode support for Common Lisp."
  :depends-on ()
  :serial t
  :components ((:file "package")
               (:file "features")
               (:file "unicode-text")
               (:file "syntax")
               (:file "test-syntax")))
