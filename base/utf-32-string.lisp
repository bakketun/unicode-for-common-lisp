(in-package #:unicode-base)

(defclass utf-32-string (code-unit-string)
  ()
  (:documentation "A code-unit-string in UTF-32 encoding form."))


(defgeneric utf-32-string (x)
  (:documentation "Converts x into utf-32-string if not already so.")
  (:method ((x utf-32-string)) x))


(defgeneric utf-32-curef (custring index)
  (:documentation "Get code-unit at index in UTF-32 encoded custring."))


(defclass standard-utf-32-string (utf-32-string)
  ((code-units :type (vector (unsigned-byte 32))
               :initarg :code-units)))


(defmethod generic-culength ((custring standard-utf-32-string))
  (length (slot-value custring 'code-units)))


(defmethod utf-32-curef ((custring standard-utf-32-string) index)
  (aref (slot-value custring 'code-units) index))


(defmethod utf-32-string (x)
  (make-instance 'standard-utf-32-string :code-units (utf-32-code-unit-vector x)))


(defmethod generic-cuencoding ((custring utf-32-string))
  :utf-32)


(defmethod generic-code-point-at ((encoding (eql :utf-32)) custring index start end)
  "Works for any object that implements utf-32-curef."
  (let ((code-point (utf-32-curef custring index)))
    (typecase code-point
      ;; type                  code-point                next index  start  error
      (scalar-value  (values   code-point                (1+ index)  index  nil))
      (t             (values   +replacement-character+   (1+ index)  index  code-point)))))
