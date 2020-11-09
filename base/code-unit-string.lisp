(in-package #:unicode-base)


(defclass code-unit-string ()
  ()
  (:documentation "A string of code units encoding Unicode text."))


(defgeneric culength (custring)
  (:documentation "Number of code units in the code-unit-string."))


(defgeneric curef (custring index)
  (:documentation "Like svref, but for code-unit-string."))


(defgeneric code-point-at (custring index)
  (:documentation "Returns the code-point (scalar value really) at index.

Values returned are:

code-point - The code point at location or #xFFFD (REPLACEMENT
CHARACTER) if there was a decoding error.

next-index - The index of the next code-point

start - The start index of the code point. Will be less than index
when it points in the middle of a well-formed code unit sequence.

error - True if there was a decoding error."))


#|
;; Internal mixin helper class

(defclass %with-code-units-vector ()
  ((%code-units :type 'vector)))

(defmethod culength ((custring %with-code-units-vector))
  (length (slot-value custring '%code-units)))

(defmethod curef ((custring %with-code-units-vector) (index fixnum))
  (aref (slot-value custring '%code-units) index))


;; Standard implementation of code-unit-string. Slot names are implementation defined.

(defclass standard-utf-8-string (utf-8-string %with-code-units-vector)
  ((%code-units :type '(vector (unsigned-byte 8)))))

(defclass standard-utf-16-string (utf-16-string %with-code-units-vector)
  ((%code-units :type '(vector (unsigned-byte 16)))))

(defclass standard-utf-32-string (utf-32-string %with-code-units-vector)
  ((%code-units :type '(vector (unsigned-byte 32)))))
|#

;; UTF-32 code-point-at

