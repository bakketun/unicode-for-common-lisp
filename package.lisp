;;;;  unicode
;;;;
;;;;  Copyright (C) 2017 Thomas Bakketun <thomas.bakketun@copyleft.no>


(defpackage #:unicode
  (:use :common-lisp)
  (:export
   :*default-unicode-type*
   :+string-unicode-type+
   :code-point
   :code-point-at
   :code-point-before
   :code-point-count
   :concatenate-unicode
   :copy-unicode
   :do-code-points
   :make-unicode
   :make-utf-8
   :make-utf-16
   :make-utf-32
   :map-code-points
   :next-code-point
   :set-code-point
   :string-code-unit
   :u8ref
   :u16ref
   :u32ref
   :unicode
   :unicode-length
   :unicode-length-for
   :unicode-ref
   :unicode-scalar
   :unicode-string
   :unicode-transform-error
   :unicode-type
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
