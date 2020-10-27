(deftype code-point-int ()
  '(integer 0 #x10FFFF))


(defparameter +named-char-map+
  (loop :for (name . code) :in '(("Space" . 32)
                                 ("Newline" . 10)
                                 ;; Semi standarc chars
                                 ("Backspace" . 8)
                                 ("Tab" . 9)
                                 ("Linefeed" . 10)
                                 ("Page" . 12)
                                 ("Return" . 13)
                                 ("Rubout" . 127)
                                 ;; More ...
                                 ("Null" . 0)
                                 )
        :for char := (name-char name)
        :when char
          :collect (cons char code)))


(deftype semi+-standard-char ()
  `(member ,@(mapcar #'car +named-char-map+)))


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
    standard-char
    semi+-standard-char
    (satisfies generic-code-point-int)))


(defun code-point-p (object)
  (typep object 'code-point))


(defparameter +graphic-standard-chars+
  "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")

(defparameter +map-code-point-int-to-char+
  (coerce
   (loop :for code-point-int :from 0 :to 127
         :for char := (if (<= 33 code-point-int 126)
                          (char +graphic-standard-chars+ (- code-point-int 33))
                          (car (find code-point-int +named-char-map+ :key #'cdr)))
         :collect char)
   'vector))


(defparameter +map-char-code-to-code-point-int+
  (map 'vector (lambda (char)
                 (and char (char-code char)))
       +map-code-point-int-to-char+))


(defun code-point-int (object)
  "Return the code point integer designated for the given object, or nil."
  (typecase object
    (integer
     object)
    (standard-code-point
     (standard-code-point-int object))
    ((member #\Newline)
     10)
    ((or standard-char semi-standard-char)
     (svref +map-char-code-to-code-point-int+ (char-code object)))
    (t
     (generic-code-point-int object))))


(defun standard-code-point (code-point)
  (etypecase code-point
    (standard-code-point
     code-point)
    (code-point
     (make-standard-code-point (code-point-int code-point)))))


(defun code-point-char (code-point)
  (svref +map-code-point-int-to-char+ (code-point-int code-point)))


(defun surrogate-code-point-p (object)
  (typep (code-point-int object) '(integer #xD800 #xDFFF)))


(deftype surrogate-code-point ()
  '(and code-point
    (satisfies surrogate-code-point-p)))


(deftype scalar-value ()
  '(and code-point
    (not surrogate-code-point)))


(defun scalar-value-p (object)
  (typep object 'scalar-value))


(defun scalar-value (object)
  (when (typep object 'scalar-value)
    (code-point-int object)))
