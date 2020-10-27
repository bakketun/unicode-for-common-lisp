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
    standard-char
    semi-standard-char
    (satisfies generic-code-point-int)))


(defun code-point-p (object)
  (typep object 'code-point))


(defparameter +graphic-and-space-standard-chars+
  " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")


(defparameter +map-char-to-code-point-int+
  (let ((h (make-hash-table)))
    ;; For implentations where #\Newline is different from #\Linefeed.
    (setf (gethash #\Newline h) 10)
    h))


(defparameter +map-code-point-int-to-char+
  (coerce
   (loop :for code-point-int :from 0 :to 127
         :for char := (if (<= 32 code-point-int 126)
                          (char +graphic-and-space-standard-chars+ (- code-point-int 32))
                          (car (find code-point-int +semi-standard-char-name-map+ :key #'cdr)))
         :when char
           :do (setf (gethash char +map-char-to-code-point-int+) code-point-int)
         :collect char)
   'vector))


(defun code-point-int (object)
  "Return the code point integer designated for the given object, or nil."
  (typecase object
    (integer
     object)
    (standard-code-point
     (standard-code-point-int object))
    ((or standard-char semi-standard-char)
     (gethash object +map-char-to-code-point-int+))
    (t
     (generic-code-point-int object))))


(defun standard-code-point (code-point)
  (etypecase code-point
    (standard-code-point
     code-point)
    (code-point
     (make-standard-code-point (code-point-int code-point)))))


(defun code-point-char (code-point)
  ;; Note: If #\Newline and #\Linefeed are different, (code-point-char 10) should return #\Linefeed.
  ;; (code-point-int #\Newline) and (code-point-int #\Linefeed) should always return 10.
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
