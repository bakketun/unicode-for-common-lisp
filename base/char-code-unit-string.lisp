(in-package #:unicode-base)


(defclass char-code-unit-string (#+string-is-utf-8  utf-8-string
                                 #+string-is-utf-16 utf-16-string
                                 #+string-is-utf-32 utf-32-string
                                 #+string-is-not-unicode code-unit-string)
  ((string :type 'string
           :initarg :string
           :reader code-unit-string-string))
  (:documentation "A string of chararacter objects as code units."))


(defgeneric string* (x)
  (:method (x) (string x))
  #-string-is-not-unicode
  (:method ((x code-unit-string))
    (map 'string
         #'code-char
         #+string-is-utf-8 (utf-8-code-unit-vector x)
         #+string-is-utf-16 (utf-16-code-unit-vector x)
         #+string-is-utf-32 (utf-32-code-unit-vector x)))
  #+string-is-not-unicode
  (:method ((custring code-unit-string))
    (let ((string (make-string (code-point-count custring)))
          (index 0))
      (map-code-points (lambda (code-point)
                         (setf (char string index) (code-point-semi-and-standard-char code-point))
                         (incf index))
                       custring)
      string)))


(defmethod custring ((custring char-code-unit-string))
  custring)


(defmethod custring ((string string))
  (make-instance 'char-code-unit-string :string string))


(defun cuchar (char-code-unit-string index)
  "Like char, but for char-code-code-unit-string objects."
  (char (code-unit-string-string char-code-unit-string) index))


(defmethod culength ((custring char-code-unit-string))
  (length (code-unit-string-string custring)))


(defmethod curef ((custring char-code-unit-string) index)
  (char-code (cuchar custring index)))


#+string-is-not-unicode
(defmethod code-point-at ((custring char-code-unit-string) index)
  (values (semi-and-standard-char-code-point (cuchar custring index))
          (1+ index)
          index
          nil))
