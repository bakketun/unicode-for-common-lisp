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
   #:code-point-decode-utf-8
   #:code-point-encode-utf-8
   #:code-point-semi-and-standard-char
   #:high-surrogate
   #:low-surrogate
   #:scalar-value
   #:semi-and-standard-char
   #:semi-and-standard-char-code-point
   #:surrogate
   ))
