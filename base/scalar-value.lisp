;;; Unicode text is a sequence of scalar values.
;;;
;;; Scalar values are allways represented by designator objects. There is no native scalar value class.
;;;
;;; Using characters as scalar values is well defined only for standard and semi standard characters.

(deftype scalar-value-int ()
  "An integer represenation of a scalar value."
  '(or
    (integer 0 #xD7FF)
    (integer #xE000 #x10FFFF)))


(defun scalar-value-int (object)
  (let ((code-point-int (code-point-int object)))
    (when (typep code-point-int 'code-point-int)
      code-point-int)))


(deftype scalar-value ()
  '(and code-point (satisfies scalar-value-int)))


(defun scalar-value-p (object)
  (typep object 'scalar-value))






