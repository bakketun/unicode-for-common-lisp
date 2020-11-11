(asdf:defsystem #:unicode-common
  :name "unicode-common"
  :licence "MIT"
  :author "Thomas Bakketun <thomas@bakketun.pro>"
  :description "Unicode common stuff"
  :depends-on ()
  :serial t
  :components ((:file "package")
               (:file "constants")
               (:file "types")
               (:file "features")
               ))
