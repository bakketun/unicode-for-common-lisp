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


(deftype semi-standard-char ()
  '(or standard-char
    (member #\Tab #\Page #\Rubout #\Linefeed #\Return #\Backspace)))


(defgeneric generic-scalar-value-p (object)
  (:documentation
   "Can be specialized to extend which objects are of type scalar-value.")
  (:method (object)
    (typep object `(or scalar-value-int
                       semi-standard-char))))


(defun scalar-value-p (object)
  "Returns true if object is of type `scalar-value`; otherwise, returns false."
  (generic-scalar-value-p object))


(deftype scalar-value ()
  '(satisfies scalar-value-p))


(defparameter +graphic-standard-chars+
  "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")

(defparameter +map-scalar-value-int-to-char+
  (coerce
   (loop :for scalar-value-int :from 0 :to 127
         :for char := (case scalar-value-int
                        (8 #\Backspace)
                        (9 #\Tab)
                        (10 #\Linefeed)
                        (12 #\Page)
                        (13 #\Return)
                        (32 #\Space)
                        (127 #\Rubout)
                        (otherwise
                         (when (<= 33 scalar-value-int 126)
                           (char +graphic-standard-chars+ (- scalar-value-int 33)))))
         :collect char)
   'vector))

(defparameter +map-char-code-to-scalar-value-int+
  (map 'vector (lambda (char)
                 (and char (char-code char)))
       +map-scalar-value-int-to-char+))

(defgeneric generic-scalar-value-int (scalar-value)
  (:documentation
   "Methods can assume that the arguement is of type scalar-value")
  (:method ((int integer))
    int)
  (:method ((char character))
    (case char
      (#\Newline 10)
      (otherwise (svref +map-char-code-to-scalar-value-int+ (char-code char))))))


(defun scalar-value-int (object)
  (check-type object scalar-value)
  (generic-scalar-value-int object))


(defun scalar-value-standard-char (scalar-value)
  "Return the semi standard character object denoting the given
scalar-value or NIL if no such character exists."
  (svref +map-scalar-value-int-to-char+ (scalar-value-int scalar-value)))


(defgeneric generic-scalar-value-char (scalar-value)
  (:method (object)
    nil))


(defun scalar-value-char (scalar-value)
  "Return the character object denoting the given scalar-value or NIL
if no such character exists.

Which scalar values that can be represented by a character is
implementation defined, except for the standard and semi standard characters."
  (or (scalar-value-standard-char scalar-value)
      (generic-scalar-value-char scalar-value)))
