(in-package #:unicode-base)


(defclass utf-32-string (code-unit-string)
  ()
  (:documentation "A code-unit-string in UTF-32 encoding form."))


(defmethod code-point-at ((custring utf-32-string) index)
  (let ((code-point (curef custring index)))
    (typecase code-point
      ;; type                  code-point                next index  start  error
      (scalar-value  (values   code-point                (1+ index)  index  nil))
      (t             (values   +replacement-character+   (1+ index)  index  code-point)))))
