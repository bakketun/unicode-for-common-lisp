(defsystem #:unicode
  :name "unicode"
  :licence "TODO"
  :author "Thomas Bakketun <thomas.bakketun@copyleft.no>"
  :description "Unicode support for Common Lisp."
  :depends-on ()
  :serial t
  :components ((:file "package")
               (:file "unicode-text")
               (:file "syntax")))



;;(pushnew :force-utf-8-strings *features*)
;;(pushnew :force-utf-16-strings *features*)
;;(pushnew :force-utf-32-strings *features*)
