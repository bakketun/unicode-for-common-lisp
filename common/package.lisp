(defpackage #:unicode-common
  (:use :common-lisp)
  (:export
   #:+bmp-code-point-limit+
   #:+code-point-limit+
   #:+first-code-point+
   #:+first-high-surrogate+
   #:+first-low-surrogate+
   #:+last-bmp-code-point+
   #:+last-code-point+
   #:+last-high-surrogate+
   #:+last-low-surrogate+
   #:+object-replacement-character+
   #:+replacement-character+
   #:bmp-code-point
   #:code-point
   #:high-surrogate
   #:low-surrogate
   #:scalar-value
   #:surrogate
   ))
