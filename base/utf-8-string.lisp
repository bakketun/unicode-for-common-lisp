(in-package #:unicode-base)


(defclass utf-8-string (code-unit-string)
  ()
  (:documentation "A code-unit-string in UTF-8 encoding form."))


(defmethod custring ((custring utf-8-string))
  custring)


(defclass standard-utf-8-string (utf-8-string byte-vector-code-unit-string)
  ((code-units :type (vector (unsigned-byte 8)))))


(defgeneric utf-8-string (x)
  (:method ((x utf-8-string)) x)
  (:method (x)
    (make-instance 'standard-utf-8-string :code-units (utf-8-code-unit-vector x))))


(defmethod code-point-at ((custring utf-8-string) index)
  (let (code-point subsequence-start subsequence-end errorp)
    (code-point-decode-utf-8 :index index
                             :start 0
                             :end (culength custring)
                             :octet (curef custring *)
                             :code-point code-point
                             :subsequence-start subsequence-start
                             :subsequence-end subsequence-end
                             :errorp errorp)
    (values (if errorp +replacement-character+ code-point) subsequence-end subsequence-start errorp)))
