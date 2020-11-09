(in-package #:unicode-base)


(defclass char-code-unit-string (#+string-is-utf-8  utf-8-string
                                 #+string-is-utf-16 utf-16-string
                                 #+string-is-utf-32 utf-32-string)
  ((string :type 'string
           :reader custring))
  (:documentation "A string of chararacter objects as code units."))


(defun cuchar (char-code-unit-string index)
  "Like char, but for char-code-code-unit-string objects."
  (char (custring char-code-unit-string) index))


(defmethod curef ((custring char-code-unit-string) index)
  (char-code (cuchar custring index)))
