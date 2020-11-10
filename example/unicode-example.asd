(asdf:defsystem #:unicode-example
  :name "unicode-example"
  :licence "MIT"
  :author "Thomas Bakketun <thomas@bakketun.pro>"
  :description "Example of how to use unicode-base etc."
  :depends-on (#:unicode-base)
  :serial t
  :components ((:file "package")
               (:file "example")))
