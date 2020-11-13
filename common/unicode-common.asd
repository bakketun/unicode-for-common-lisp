(asdf:defsystem #:unicode-common
  :name "unicode-common"
  :licence "MIT"
  :author "Thomas Bakketun <thomas@bakketun.pro>"
  :description "Unicode common stuff"
  :depends-on ()
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "constants")
               (:file "types")
               (:file "standard-char")
               (:file "features")
               (:file "utf-8")
               (:file "utf-16")
               ))
