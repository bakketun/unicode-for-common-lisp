(in-package #:unicode-base)


(defmethod generic-cuencoding ((string string))
  #+string-is-utf-8 :utf-8
  #+string-is-utf-16 :utf-16
  #+string-is-utf-32 :utf-32
  #+string-is-not-unicode 'standard-char)


(defmethod generic-culength ((string string))
  (length string))


#+string-is-utf-8
(defmethod utf-8-curef ((string string) index)
  (char-code (char string index)))


#+string-is-utf-16
(defmethod utf-16-curef ((string string) index)
  (char-code (char string index)))


#+string-is-utf-32
(defmethod utf-32-curef ((string string) index)
  (char-code (char string index)))


#+string-is-not-unicode
(defmethod generic-code-point-at ((encoding (eql 'standard-char)) (string string) index start end)
  (values (semi-and-standard-char-code-point (char string index))
          (1+ index)
          index
          nil))


(defun unicode-to-string (custring)
  "Convert custring into cl:string"
  (typecase custring
    (string custring)
    (t
     #-string-is-not-unicode
     (map 'string
          #'code-char
          #+string-is-utf-8 (utf-8-code-unit-vector custring)
          #+string-is-utf-16 (utf-16-code-unit-vector custring)
          #+string-is-utf-32 (utf-32-code-unit-vector custring))
     #+string-is-not-unicode
     (let ((string (make-string (code-point-count custring)))
           (index 0))
       (map-code-points (lambda (code-point)
                          (setf (char string index) (code-point-semi-and-standard-char code-point))
                          (incf index))
                        custring)
       string))))
