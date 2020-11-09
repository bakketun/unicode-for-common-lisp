(in-package #:unicode-base)


(defclass byte-vector-code-unit-string ()
  ((code-units :type 'vector
               :initarg :code-units
               :reader code-unit-string-vector))
  (:documentation "Abstract super class for code-unit-string using a byte vector of 8, 16 or 32 bit as backing storage."))


(defmethod culength ((custring byte-vector-code-unit-string))
  (length (slot-value custring 'code-units)))


(defmethod curef ((custring byte-vector-code-unit-string) (index fixnum))
  (aref (slot-value custring 'code-units) index))
