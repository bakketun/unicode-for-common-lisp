(in-package #:unicode-base)


(defclass char-code-unit-string (#+string-is-utf-8  utf-8-string
                                 #+string-is-utf-16 utf-16-string
                                 #+string-is-utf-32 utf-32-string)
  ((string :type 'string
           :initarg :string
           :reader code-unit-string-string))
  (:documentation "A string of chararacter objects as code units."))


(defgeneric string* (x)
  (:method ((x string)) x)
  (:method (x)
    (map 'string
         #'code-char
         #+string-is-utf-8 (utf-8-code-unit-vector x)
         #+string-is-utf-16 (utf-16-code-unit-vector x)
         #+string-is-utf-32 (utf-32-code-unit-vector x))))


(defmethod custring ((string string))
  (make-instance 'char-code-unit-string :string string))


(defun cuchar (char-code-unit-string index)
  "Like char, but for char-code-code-unit-string objects."
  (char (code-unit-string-string char-code-unit-string) index))


(defmethod culength ((custring char-code-unit-string))
  (length (code-unit-string-string custring)))


(defmethod curef ((custring char-code-unit-string) index)
  (char-code (cuchar custring index)))
