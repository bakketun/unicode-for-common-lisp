(defun scalar-value (object)
  (let ((code-point-int (code-point-int object)))
    (unless (typep code-point-int '(integer #xD800 #xDFFF))
      code-point-int)))


(deftype scalar-value ()
  '(and code-point (satisfies scalar-value)))


(defun scalar-value-p (object)
  (typep object 'scalar-value))






