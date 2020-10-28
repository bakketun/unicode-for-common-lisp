(deftype code-point-int ()
  '(integer 0 #x10FFFF))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +semi-standard-char-name-map+
    (loop :for (name . code) :in '(("Backspace" . 8)
                                   ("Tab" . 9)
                                   ("Linefeed" . 10)
                                   ("Page" . 12)
                                   ("Return" . 13)
                                   ("Space" . 32)
                                   ("Rubout" . 127))
          :for char := (name-char name)
          :when char
            :collect (cons char code))))


(deftype semi-standard-char ()
  `(member ,@(mapcar #'car +semi-standard-char-name-map+)))


(defstruct (standard-code-point
            (:constructor make-standard-code-point (int)))
  (int nil :type code-point-int :read-only t))


(defgeneric generic-code-point-int (code-point)
  (:documentation
   "Methods can assume that the arguement is of type code-point")
  (:method (object)
    nil))


(deftype code-point ()
  '(or
    code-point-int
    standard-code-point
    (and standard-char
     (not (member #\Newline)))
    semi-standard-char
    (satisfies generic-code-point-int)))


(defparameter +graphic-and-space-standard-chars+
  " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")


(defparameter +map-char-to-code-point-int+
  (make-hash-table))


(defparameter +map-code-point-int-to-char+
  (coerce
   (loop :for code-point-int :from 0 :to 127
         :for char := (typecase code-point-int
                        ((integer 32 126)
                         (char +graphic-and-space-standard-chars+ (- code-point-int 32)))
                        (t
                         (car (find code-point-int +semi-standard-char-name-map+ :key #'cdr))))
         :when char
           :do (setf (gethash char +map-char-to-code-point-int+) code-point-int)
         :collect char)
   'vector))


(defun code-point-p (object)
  "If object type code-point returns the integer value of the code point, othewise NIL.

Note: #\Newline might or might not map to a code-point."
  (typecase object
    (code-point-int
     object)
    (standard-code-point
     (standard-code-point-int object))
    ((or standard-char semi-standard-char)
     (values (gethash object +map-char-to-code-point-int+)))
    (t
     (generic-code-point-int object))))


(defun code-point-code (object)
  (check-type object code-point)
  (code-point-p object))


(defun standard-code-point (code-point)
  (etypecase code-point
    (standard-code-point
     code-point)
    (code-point
     (make-standard-code-point (code-point-code code-point)))))


(defun code-point-char (code-point)
  (svref +map-code-point-int-to-char+ (code-point-code code-point)))


(defun surrogate-code-point-p (object)
  (when (typep (code-point-code object) '(integer #xD800 #xDFFF))
    object))


(deftype surrogate-code-point ()
  '(and code-point
    (satisfies surrogate-code-point-p)))


(deftype scalar-value ()
  '(and code-point
    (not surrogate-code-point)))


(defun scalar-value (object)
  (check-type object scalar-value)
  (scalar-value-p object))


(defun scalar-value-p (object)
  (when (typep object 'scalar-value)
    (code-point-code object)))
