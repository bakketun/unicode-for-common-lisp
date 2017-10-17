;;;;  unicode
;;;;
;;;;  Copyright (C) 2017 Thomas Bakketun <thomas.bakketun@copyleft.no>


(defpackage #:unicode
  (:use :common-lisp)
  (:export
   :code-point
   :code-point-count
   :do-code-points
   :make-unicode
   :map-code-points
   :next-code-point
   :set-code-point
   :u8ref
   :u16ref
   :u32ref
   :unicode
   :unicode-length
   :unicode-length-for
   :unicode-scalar
   :unicode-string
   :unicode-to-string
   :unicode-transform-error
   :unicodep
   :utf-8
   :utf-8-code-unit
   :utf-8-length
   :utf-8-p
   :utf-16
   :utf-16-code-unit
   :utf-16-length
   :utf-16-p
   :utf-32
   :utf-32-code-unit
   :utf-32-length
   :utf-32-p
   ))
