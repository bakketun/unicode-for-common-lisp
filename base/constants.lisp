(in-package #:unicode-base)

(defconstant +replacement-character+ #xFFFD)
(defconstant +object-replacement-character+ #xFFFC)

(defconstant +first-code-point+ 0)
(defconstant +last-code-point+ #x10FFFF)
(defconstant +code-point-limit+ (1+ +last-code-point+))

(defconstant +last-bmp-code-point+ #xFFFF)
(defconstant +bmp-code-point-limit+ (1+ +last-bmp-code-point+))

(defconstant +first-high-surrogate+ #xD800)
(defconstant +last-high-surrogate+ #xDBFF)
(defconstant +first-low-surrogate+ #xDC00)
(defconstant +last-low-surrogate+ #xDFFF)
